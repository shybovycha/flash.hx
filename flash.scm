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
        ; INFO: +1 in this case offsets searches so that the second "a" in "maa" would be skipped when searching for "a"; this could be made configurable
        (find-matches-rec pattern (substring source (+ p-len 1)) (+ pos p-len 1) (append acc (list pos)))
        (find-matches-rec pattern (substring source 1) (+ pos 1) acc)))))

(define (find-matches pattern source)
  (find-matches-rec pattern source 0 '()))

(define (find-first f lst)
  (if (or (not (list? lst)) (empty? lst))
      #f
      (if (f (first lst))
          (first lst)
          (find-first f (rest lst)))))

;; ----------------------------------------

(define (default-flash-state) (hash 'input "" 'cursor-to-be #f 'matches '()))
(define *flash-state* (default-flash-state))

(define (flash-update-status)
  (let* ([input (hash-ref *flash-state* 'input)])
    (set-status! (to-string "flash:" input))))

(define (map-matches idx input line)
  (map (lambda (ms) (append (list ms) (list idx))) (find-matches input line)))

; TODO: filter labels by the last letter of the match
(define (matches-and-labels input lines as)
  (transduce
    lines
    (compose
      (enumerating)
      (flat-mapping (lambda (a) (map-matches (first a) input (second a)))) ; inverted arguments because x is the _column_ and y is _line index_
      (taking (length as))
      (zipping as)
      (mapping (lambda (a) (append (first a) (list (second a)))))) ; produces (x y jump-label)
    (into-list)))

(define (flash-first-cursor-pos-on-screen)
  (if (and (list? (current-cursor)) (not (empty? (current-cursor))) (Position? (first (current-cursor))))
           (first (current-cursor))
           #f))

; INFO: reading from config does not work https://github.com/helix-editor/helix/pull/8675/commits/b7725c818708369cb42f03538625d2bbea01a948
(define (flash-jump-label-alphabet)
  (map integer->char (range (char->integer #\a) (char->integer #\z))))

; INFO: moving cursor to a specified position is impossible otherwise
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

(define (flash-render state rect frame)
  (let*
    ([cursor-on-screen (flash-first-cursor-pos-on-screen)]
     [cursor-in-buffer (position (helix.static.get-current-line-number) (helix.static.get-current-column-number))]
     [cursor-line-on-screen (position-row cursor-on-screen)]
     [cursor-line-in-buffer (position-row cursor-in-buffer)]
     [cursor-column-in-buffer (position-col cursor-in-buffer)]
     [cursor-column-on-screen (position-col cursor-on-screen)]
     [gutter (- cursor-column-on-screen cursor-column-in-buffer)]
     [lines-on-screen (area-height rect)]
     [alphabet (flash-jump-label-alphabet)]
     [full-text (editor->text (editor->doc-id (editor-focus)))]
     ; TODO: either skip the text before cursor and offset first line matches accordingly, or use an entire screen worth of text
     [lines (map (lambda (n) (rope->string (rope->line full-text n))) (range cursor-line-in-buffer (+ 1 cursor-line-in-buffer (- lines-on-screen cursor-line-on-screen))))]
     [input (hash-ref *flash-state* 'input)]
     [label-style (style-with-bold (theme-scope "ui.cursor.match"))]
     [match-style (style-with-italics (theme-scope "ui.text.focus"))])
    (if (> (string-length input) 0)
        (let*
          ([matches (matches-and-labels input lines alphabet)])
          (begin
           (map
             (lambda (a) (let*
                     ([match-x (first a)]
                      [match-y (second a)]
                      [label (string (third a))]
                      [match-row (+ cursor-line-on-screen match-y)]
                      [match-col (+ gutter (string-length input) match-x)])
                     (begin
                       (frame-set-string! frame match-col match-row label label-style)
                       (map (lambda (x) (frame-set-string! frame (+ gutter match-x x) match-row (string (string-ref input x)) match-style)) (range (string-length input))))))
              matches)
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
        [found-match (find-first (lambda (m) (char=? key-char (third m))) matches)])
       (if found-match
          (begin
            ; INFO: this does not work either
            ; (set! *flash-state* (hash-insert *flash-state* 'cursor-to-be '(3 3)))
            ; INFO: there is no reasonable way to move cursor to a position and then pop the component
            ; (helix.static.goto_line (first found-match)) ; goto_line does not have arguments... WAT?!
            ; (helix.static.goto_column (second found-match)) ; goto_column also does not have arguments. WAT?!
            (move-cursor-by (second found-match) (+ (- (first found-match) (helix.static.get-current-column-number)) 1))
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

