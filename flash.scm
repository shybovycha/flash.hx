(require "helix/editor.scm")
(require (prefix-in helix.commands. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(require "helix/components.scm")
(require "helix/misc.scm")
(require "helix/configuration.scm")
(require-builtin helix/core/text)
(require-builtin steel/lists)
(require-builtin steel/strings)

(define (find-matches-rec pattern source pos acc) 
  (let* ([p-len (string-length pattern)]
         [s-len (string-length source)])
    (if (> p-len s-len)
       acc
       (if (starts-with? source pattern)
        (find-matches-rec pattern (substring source p-len) (+ pos p-len) (append acc (list pos)))
        (find-matches-rec pattern (substring source 1) (+ pos 1) acc)))))

(define (find-matches pattern source)
  (find-matches-rec pattern source 0 '()))

(define (find-first f lst)
  (if (or (not (list? lst)) (empty? lst))
      #f
      (if (f (first lst))
          (first lst)
          (find-first f (rest lst)))))

(define (zip l1 l2)
  (let*
    ([len (min (length l1) (length l2))])
    (map (lambda (i) (list (list-ref l1 i) (list-ref l2 i))) (range len))))

;; ----------------------------------------

(define (default-flash-state) (hash 'input "" 'cursor-to-be #f 'matches '()))
(define *flash-state* (default-flash-state))

(define (flash-update-status)
  (let* ([input (hash-ref *flash-state* 'input)])
    (set-status! (to-string "flash:" input))))

(define (map-matches idx input line)
  (map
    (lambda (ms) (hash 'col ms 'row idx 'next-char (string-ref line (+ ms (string-length input)))))
    (find-matches input line)))

(define softwrap-width 2)

(define (pack-into-sub width lines line-idx)
  (if (empty? lines) 
    '()
    (let*
      ([first-line (first lines)]
       [s-line (hash-ref first-line 'line)]
       [len (string-length s-line)]
       [matches (hash-ref first-line 'matches)])
      (if (<= len width)
        (append 
          (list 
            (~> first-line
              (hash-insert 'idx line-idx)
              (hash-insert 'matches (map (lambda (m) (hash 'row line-idx 'col m 'char-idx m)) matches))))
          (pack-into-sub width (rest lines) (+ 1 line-idx)))
        (append
          (list 
            (~> first-line
              (hash-insert 'matches (map (lambda (m) (hash 'char-idx m 'row (+ line-idx (floor (/ m width))) 'col (if (> m width) (+ softwrap-width (modulo m width)) m))) matches))
              (hash-insert 'idx line-idx)))
          (pack-into-sub width (rest lines) (+ (ceiling (/ len width)) line-idx)))))))

(define (pack-into width lines)
  (if (or (not (list? lines)) (empty? lines))
    #f
    (pack-into-sub width lines 0)))

(define (matches-and-labels input lines as max-line-length)
  (let*
    ([lines-with-matches (map (lambda (l) (hash 'line l 'matches (find-matches input l))) lines)]
     [lines-with-positions (pack-into max-line-length lines-with-matches)]
     [matches-with-positions
       (flatten
         (map
           (lambda (l)
                   (map
                     (lambda (m)
                             (hash 'idx (hash-ref l 'idx) 'row (hash-ref m 'row) 'col (hash-ref m 'col) 'next-char (string-ref (hash-ref l 'line) (+ 1 (hash-ref m 'char-idx)))))
                     (hash-ref l 'matches)))
           lines-with-positions))]
     [next-chars (list->hashset (map (lambda (l) (hash-ref l 'next-char)) matches-with-positions))]
     [alphabet (filter (lambda (a) (not (hashset-contains? next-chars a))) as)])
    (transduce
      matches-with-positions
      (compose
        (taking (length alphabet))
        (zipping alphabet)
        (mapping (lambda (a) (hash-insert (first a) 'label (second a)))))
      (into-list))))

(define (flash-first-cursor-pos-on-screen)
  (if (and (list? (current-cursor)) (not (empty? (current-cursor))) (Position? (first (current-cursor))))
           (first (current-cursor))
           #f))

; INFO: reading from config does not work https://github.com/helix-editor/helix/pull/8675/commits/b7725c818708369cb42f03538625d2bbea01a948
(define (flash-jump-label-alphabet)
  (map integer->char (range (char->integer #\a) (char->integer #\z))))

; INFO: moving cursor to a specified position is impossible otherwise
; INFO: this does not work:
; (set! *flash-state* (hash-insert *flash-state* 'cursor-to-be '(3 3)))
; INFO: there is no reasonable way to move cursor to a position and then pop the component
; (helix.static.goto_line (first found-match)) ; goto_line does not have arguments... WAT?!
; (helix.static.goto_column (second found-match)) ; goto_column also does not have arguments. WAT?!
(define (move-cursor-by rows cols)
  (if (and (= 0 rows) (= 0 cols))
      #t
      (begin
        (cond
          [(> rows 0) (helix.static.move_line_down) (move-cursor-by (- rows 1) cols)]
          [(< rows 0) (helix.static.move_line_up) (move-cursor-by (+ rows 1) cols)]
          [(> cols 0) (helix.static.move_char_right) (move-cursor-by rows (- cols 1))]
          [(< cols 0) (helix.static.move_char_left) (move-cursor-by rows (+ cols 1))]
          [else #t]))))

(define (flash-get-lines-forward cursor-line-in-buffer)
  (let*
    ([full-text (rope->string (editor->text (editor->doc-id (editor-focus))))]
     ; TODO: either skip the text before cursor and offset first line matches accordingly, or use an entire screen worth of text
     ; TODO: limit the number of lines on the screen / in the buffer
     ; [lines-on-screen (area-height rect)]
     [lines (list-drop (split-many full-text "\n") cursor-line-in-buffer)] ;(map (lambda (n) (rope->string (rope->line full-text n))) (range cursor-line-in-buffer (+ 1 cursor-line-in-buffer (- lines-on-screen cursor-line-on-screen))))]
    )
    lines))

(define (flash-get-gutter)
  (let*
    ([cursor-on-screen (flash-first-cursor-pos-on-screen)]
     [cursor-in-buffer (position (helix.static.get-current-line-number) (helix.static.get-current-column-number))]
     [cursor-column-in-buffer (position-col cursor-in-buffer)]
     [cursor-column-on-screen (position-col cursor-on-screen)]
     [gutter (- cursor-column-on-screen cursor-column-in-buffer)])
    gutter))

(define (flash-render-jump-labels frame matches input)
  (let*
    ([cursor-on-screen (flash-first-cursor-pos-on-screen)]
     [cursor-line-on-screen (position-row cursor-on-screen)]
     [gutter (flash-get-gutter)]
     [label-style (style-with-bold (theme-scope "ui.cursor.match"))]
     [match-style (style-with-italics (theme-scope "ui.text.focus"))]
     )
  (map
    (lambda (a)
      (let*
        ([match-x (hash-ref a 'col)]
         [match-y (hash-ref a 'row)]
         [label (string (hash-ref a 'label))]
         [match-row (+ cursor-line-on-screen match-y)]
         [match-col (+ gutter (string-length input) match-x)])
         (begin
           (frame-set-string! frame match-col match-row label label-style)
           ; INFO: this works extremely poorly with wrapped lines (soft-wraps)
           ; TODO: need to account for gutter + line width + line-wrap string (character?) and accumulate over all lines
           (map (lambda (x) (frame-set-string! frame (+ gutter match-x x) match-row (string (string-ref input x)) match-style)) (range (string-length input))))))
    matches)))

(define (flash-render state rect frame)
  (let*
    ([cursor-line-in-buffer (helix.static.get-current-line-number)]
     [alphabet (flash-jump-label-alphabet)]
     [lines (flash-get-lines-forward cursor-line-in-buffer)]
     [input (hash-ref *flash-state* 'input)]
     [max-line-width (- (area-width rect) (flash-get-gutter) 1)])
    (if (> (string-length input) 0)
        (let*
          ([matches (matches-and-labels input lines alphabet max-line-width)])
          (begin
           (flash-render-jump-labels frame matches input)
           (flash-update-status)
           (set! *flash-state* (hash-insert *flash-state* 'matches matches))
           )))))

(define (flash-remove-last-input-char)
  (let*
       ([input (hash-ref *flash-state* 'input)]
        [input-len (string-length input)])
       (when (> input-len 0)
         (set! *flash-state* (hash-insert *flash-state* 'input (substring input 0 (- input-len 1)))))))

(define (flash-append-input-char key-char)
  (let*
    ([input (hash-ref *flash-state* 'input)]
     [new-input (apply string (append (string->list input) (list key-char)))])
    (set! *flash-state* (hash-insert *flash-state* 'input new-input))))

(define (flash-handle-event state event)
  (define key-char (key-event-char event))
  (cond
    [(key-event-escape? event)
     (set-status! "")
     event-result/close]
    [(key-event-backspace? event)
     (flash-remove-last-input-char)
     (flash-update-status)
     event-result/consume]
    [(char? key-char)
     (let*
       ([matches (hash-ref *flash-state* 'matches)]
        [found-match (find-first (lambda (m) (char=? key-char (hash-ref m 'label))) matches)])
       (if found-match
          (begin
            (move-cursor-by (hash-ref found-match 'row) (+ (- (hash-ref found-match 'col) (helix.static.get-current-column-number)) 1))
            (set-status! "")
            event-result/close)
          (begin
            (flash-append-input-char key-char)
            (flash-update-status)
            event-result/consume)))]
    [else
      (enqueue-thread-local-callback (lambda () void))
      event-result/ignore]))

(define (flash-handle-cursor-event state event) #f)

(define (flash)
  (begin
    (set! *flash-state* (default-flash-state))
    (push-component!
      (new-component!
        "flash"
        *flash-state*
        flash-render
        (hash
          "handle_event" flash-handle-event
          "cursor" flash-handle-cursor-event)))))

(provide flash)

