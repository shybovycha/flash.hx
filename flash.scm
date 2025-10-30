(require "helix/editor.scm")
(require (prefix-in helix.commands. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(require "helix/components.scm")
(require "helix/misc.scm")
(require "helix/configuration.scm")
(require "helix/ext.scm")
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

(define (find-matches-forward pattern source offset)
  (find-matches-rec pattern (if (number? offset) (substring source offset) source) (if (number? offset) offset 0) '()))

(define (find-matches-backward pattern source offset)
  (find-matches-rec pattern (if (number? offset) (substring source 0 offset) source) 0 '()))

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

(define (default-flash-state) (hash 'input "" 'matches '() 'direction 'forward))
(define *flash-state* (default-flash-state))

(define (flash-update-status)
  (let* ([input (hash-ref *flash-state* 'input)])
    (set-status! (to-string "flash:" input))))

(define softwrap-width 2)

(define (align-matches-with-softwraps-sub width lines direction line-offset line-idx)
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
              (hash-insert 'matches (map (lambda (m) (hash 'row line-offset 'col m 'char-idx m)) matches))
              (hash-insert 'line-idx line-idx)))
          (align-matches-with-softwraps-sub width (rest lines) direction (+ 1 line-offset) (if (equal? 'backward direction) (- line-idx 1) (+ 1 line-idx))))
        (append
          (list 
            (~> first-line
              ; TODO: softwrap happens on the edge of word and non-word character, making it non-uniform line width :sadpanda:
              (hash-insert 'matches (map (lambda (m) (hash 'char-idx m 'row (+ line-offset (floor (/ m width))) 'col (if (> m width) (+ softwrap-width (modulo m width)) m))) matches))
              (hash-insert 'line-idx line-idx)))
          (align-matches-with-softwraps-sub width (rest lines) direction (+ (ceiling (/ len width)) line-offset) (if (equal? 'backward direction) (- line-idx 1) (+ 1 line-idx))))))))

(define (align-matches-with-softwraps width lines direction)
  (if (or (not (list? lines)) (empty? lines))
    #f
    (align-matches-with-softwraps-sub
      width
      lines
      direction
      0
      0)))

(define (unpack-matches-sub entries acc)
  (if (empty? entries)
    acc
    (let*
      ([entry (first entries)]
       [line (hash-ref entry 'line)]
       [line-idx (hash-ref entry 'line-idx)]
       [matches (hash-ref entry 'matches)]
       [new-matches
         (map
           (lambda (m)
                   (hash
                     'row (hash-ref m 'row)
                     'col (hash-ref m 'col)
                     'line-idx line-idx
                     'char-idx (hash-ref m 'char-idx)
                     'next-char (string-ref line (min (- (string-length line) 1) (+ (hash-ref m 'char-idx) 1)))))
           matches)])
      (unpack-matches-sub (rest entries) (append acc new-matches)))))

(define (unpack-matches matches)
  (if (or (not (list? matches)) (empty? matches))
      '()
      (unpack-matches-sub matches '())))

(define (assign-labels matches alphabet)
  (let*
     ([next-letters (map (lambda (m) (hash-ref m 'next-char)) matches)]
      [next-letters-set (list->hashset next-letters)]
      [available-letters (filter (lambda (c) (not (hashset-contains? next-letters-set c))) alphabet)])
     (map (lambda (m) (hash-insert (first m) 'label (second m))) (zip matches available-letters))))

(define (find-matches-with-offset input lines offset direction)
  (if (or (not (list? lines)) (empty? lines))
      '()
      (let*
        ([first-line (first lines)]
         [rest-lines (rest lines)])
         (if (number? offset)
            (append
              (list
                (hash
                  'line first-line
                  'matches (if (equal? 'backward direction)
                    (find-matches-backward input first-line offset)
                    (find-matches-forward input first-line offset))))
              (find-matches-with-offset input rest-lines #f direction))
            (append
              (list
                (hash
                  'line first-line
                  'matches (if (equal? 'backward direction)
                               (find-matches-backward input first-line #f)
                               (find-matches-forward input first-line #f))))
              (find-matches-with-offset input rest-lines #f direction))))))

(define (matches-and-labels input lines max-line-length cursor-in-buffer cursor-on-screen direction)
  (let*
    ([initial-offset (if (equal? 'everywhere direction) #f (max 0 (- (position-col cursor-in-buffer) 1)))]
     [alphabet (flash-jump-label-alphabet)]
     [lines-with-matches (find-matches-with-offset input lines initial-offset direction)]
     [lines-with-positions (align-matches-with-softwraps max-line-length lines-with-matches direction)]
     [flat-matches (unpack-matches lines-with-positions)])
    (assign-labels flat-matches alphabet)))

(define (flash-first-cursor-pos-on-screen)
  (if (and (list? (current-cursor)) (not (empty? (current-cursor))) (Position? (first (current-cursor))))
           (first (current-cursor))
           #f))

(define (flash-jump-label-alphabet)
  (let*
    ([configured-alphabet (get-config-option-value "jump-label-alphabet")])
    (if (or (not (string? configured-alphabet)) (not (> (string-length configured-alphabet) 0)))
        (map integer->char (range (char->integer #\a) (char->integer #\z)))
        configured-alphabet)))

(define (flash-pick-lines-forward-sub lines offset max-lines line-width)
  (if (or (>= offset max-lines) (empty? lines))
      '()
      (append
        (list (first lines))
        (flash-pick-lines-forward-sub
          (rest lines)
          (+ offset (ceiling (/ (max 1 (string-length (first lines))) line-width)))
          max-lines
          line-width))))

(define (flash-pick-lines-forward lines cursor-in-buffer cursor-on-screen max-lines line-width)
  (let*
    ([lines2 (drop lines (position-row cursor-in-buffer))]
     [offset (floor (/ (position-col cursor-on-screen) line-width))])
    (flash-pick-lines-forward-sub lines2 offset max-lines line-width)))

(define (flash-pick-lines-backward-sub lines offset max-lines line-width)
  (if (or (>= offset max-lines) (empty? lines))
      '()
      (append
        (list (first lines))
        (flash-pick-lines-backward-sub
          (rest lines)
          (+ offset (ceiling (/ (max 1 (string-length (first lines))))))
          max-lines
          line-width))))

(define (flash-pick-lines-backward lines cursor-in-buffer cursor-on-screen max-lines line-width)
  (let*
    ([lines2 (reverse (take lines (+ 1 (position-row cursor-in-buffer))))]
     [trimmed-lines (append (list (substring (first lines2) 0 (position-col cursor-in-buffer))) (rest lines2))]
     [offset (floor (/ (position-col cursor-on-screen) line-width))]
     [max-lines (- max-lines (position-row cursor-on-screen))])
    (flash-pick-lines-backward-sub trimmed-lines offset max-lines line-width)))

(define (flash-get-lines-forward cursor-in-buffer cursor-on-screen max-lines line-width)
  (let*
    ([full-text (rope->string (editor->text (editor->doc-id (editor-focus))))]
     [lines (split-many full-text "\n")])
    (flash-pick-lines-forward lines cursor-in-buffer cursor-on-screen max-lines line-width)))

(define (flash-get-lines-backward cursor-in-buffer cursor-on-screen max-lines line-width)
  (let*
    ([full-text (rope->string (editor->text (editor->doc-id (editor-focus))))]
     [lines (split-many full-text "\n")])
    (flash-pick-lines-backward lines cursor-in-buffer cursor-on-screen max-lines line-width)))

(define (flash-get-lines-on-screen cursor-in-buffer cursor-on-screen screen-height line-width)
  (let*
    ([full-text (rope->string (editor->text (editor->doc-id (editor-focus))))]
     [lines (split-many full-text "\n")]
     [lines-back (flash-pick-lines-backward lines cursor-in-buffer cursor-on-screen screen-height line-width)]
     [lines-forward (flash-pick-lines-forward lines cursor-in-buffer cursor-on-screen screen-height line-width)])
    (append lines-back lines-forward)))

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
     [input-len (string-length input)])
  (map
    (lambda (a)
      (let*
        ([match-x (hash-ref a 'col)]
         [match-y (hash-ref a 'row)]
         [label (string (hash-ref a 'label))]
         [match-row (if (equal? 'backward (hash-ref *flash-state* 'direction))
                      (- cursor-line-on-screen match-y)
                      (+ cursor-line-on-screen match-y))]
         [match-col (+ gutter input-len match-x)])
         (begin
           (frame-set-string! frame match-col match-row label label-style)
           ; INFO: this works extremely poorly with wrapped lines (soft-wraps)
           ; TODO: need to account for gutter + line width + line-wrap string (character?) and accumulate over all lines
           (map (lambda (x) (frame-set-string! frame (+ gutter match-x x) match-row (string (string-ref input x)) match-style)) (range input-len)))))
    matches)))

(define (flash-render state rect frame)
  (if (> (string-length (hash-ref *flash-state* 'input)) 0)
    (let*
      ([cursor-line-in-buffer (helix.static.get-current-line-number)]
       [cursor-column-in-buffer (helix.static.get-current-column-number)]
       [cursor-in-buffer (position cursor-line-in-buffer cursor-column-in-buffer)]
       [cursor-on-screen (flash-first-cursor-pos-on-screen)]
       [cursor-line-on-screen (position-row cursor-on-screen)]
       [max-line-width (- (area-width rect) (flash-get-gutter) 1)]
       [direction (hash-ref *flash-state* 'direction)]
       [lines (cond
                [(equal? 'backward direction) (flash-get-lines-backward cursor-in-buffer cursor-on-screen (area-height rect) max-line-width)]
                [(equal? 'forward direction) (flash-get-lines-forward cursor-in-buffer cursor-on-screen (area-height rect) max-line-width)]
                [else (flash-get-lines-on-screen cursor-in-buffer cursor-on-screen (area-height rect) max-line-width)])]
       [input (hash-ref *flash-state* 'input)]
       [matches (matches-and-labels input lines max-line-width cursor-in-buffer cursor-on-screen direction)])
      (begin
       (flash-render-jump-labels frame matches input)
       (flash-update-status)
       (set! *flash-state* (hash-insert *flash-state* 'matches matches))))))

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
            (set-status! "")
            (helix.commands.goto-line (+ 1 (helix.static.get-current-line-number) (hash-ref found-match 'line-idx)) (equal? (editor-mode) "select"))
            (helix.commands.goto-column (+ (max 0 (- (string-length (hash-ref *flash-state* 'input)) 1)) (hash-ref found-match 'char-idx)) (equal? (editor-mode) "select"))
            event-result/close)
          (begin
            (flash-append-input-char key-char)
            (flash-update-status)
            event-result/consume)))]
    [(key-event? event) event-result/close]
    [else event-result/ignore]))

(define (flash-handle-cursor-event state event) #f)

(define (flash-init)
  (push-component!
      (new-component!
        "flash"
        *flash-state*
        flash-render
        (hash
          "handle_event" flash-handle-event
          "cursor" flash-handle-cursor-event))))

;;@doc
; Prefix search on screen and jump
(define (flash)
  (begin
    (set! *flash-state* (hash-insert (default-flash-state) 'direction 'everywhere))
    (set-status! "flash:")
    (flash-init)))

;;@doc
; Prefix search backward and jump
(define (flash-backward)
  (begin
    (set! *flash-state* (hash-insert (default-flash-state) 'direction 'backward))
    (set-status! "flash-back:")
    (flash-init)))

;;@doc
; Prefix search forward and jump
(define (flash-forward)
  (begin
    (set! *flash-state* (hash-insert (default-flash-state) 'direction 'forward))
    (set-status! "flash-forward:")
    (flash-init)))

(provide flash
         flash-backward
         flash-forward)

