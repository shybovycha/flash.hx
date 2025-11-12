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

(define (default-flash-state) (hash 'input "" 'matches '() 'direction 'forward 'extend #f))
(define *flash-state* (default-flash-state))

(define (flash-update-status)
  (let*
    ([input (hash-ref *flash-state* 'input)]
     [direction (hash-ref *flash-state* 'direction)]
     [should-extend (hash-ref *flash-state* 'extend)]
     [direction-s (cond
                    [(equal? 'backward direction) "[backward]"]
                    [(equal? 'forward direction) "[forward]"]
                    [else #f])]
     [extend-s (if should-extend "[extend]" #f)]
     [status (string-append (string-join (filter (lambda (x) x) (list "flash" direction-s extend-s)) " ") ":")])
    (set-status! (to-string status input))))

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
          (align-matches-with-softwraps-sub
            width
            (rest lines)
            direction
            (if (equal? 'backward direction)
                (- line-offset 1)
                (+ 1 line-offset))
            (if (equal? 'backward direction)
                (- line-idx 1)
                (+ 1 line-idx))))
        (append
          (list 
            (~> first-line
              ; TODO: softwrap happens on the edge of word and non-word character, making it non-uniform line width :sadpanda:
              (hash-insert
                'matches
                (map
                  (lambda (m)
                          (hash
                            'char-idx m
                            'row (if (equal? 'backward direction)
                                     (- line-offset (floor (/ m width)))
                                     (+ line-offset (floor (/ m width))))
                            'col (if (> m width)
                                     (+ softwrap-width (modulo m width))
                                     m)))
                  matches))
              (hash-insert 'line-idx line-idx)))
          (align-matches-with-softwraps-sub
            width
            (rest lines)
            direction
            (if (equal? 'backward direction)
                (- line-offset (ceiling (/ len width)))
                (+ (ceiling (/ len width)) line-offset))
            (if (equal? 'backward direction)
                (- line-idx 1)
                (+ 1 line-idx))))))))

(define (align-matches-with-softwraps width lines direction)
  (if (or (not (list? lines)) (empty? lines))
    #f
    (align-matches-with-softwraps-sub
      width
      lines
      direction
      0
      0)))

(define (unpack-matches-sub input entries acc)
  (if (empty? entries)
    acc
    (let*
      ([entry (first entries)]
       [line (hash-ref entry 'line)]
       [line-idx (hash-ref entry 'line-idx)]
       [matches (hash-ref entry 'matches)]
       [input-len (string-length input)]
       [matches1 (filter (lambda (m) (< (+ (hash-ref m 'char-idx) input-len) (string-length line))) matches)]
       [new-matches
         (map
           (lambda (m)
                   (hash
                     'row (hash-ref m 'row)
                     'col (hash-ref m 'col)
                     'line-idx line-idx
                     'char-idx (hash-ref m 'char-idx)
                     'next-char (string-ref line (+ (hash-ref m 'char-idx) input-len))))
           matches1)])
      (unpack-matches-sub input (rest entries) (append acc new-matches)))))

(define (unpack-matches input matches)
  (if (or (not (list? matches)) (empty? matches))
      '()
      (unpack-matches-sub input matches '())))

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

(define (flash-flat-matches input lines max-line-length cursor-in-buffer cursor-on-screen direction)
  (if (empty? lines)
      '()
      (let*
        ([initial-offset (if (equal? 'everywhere direction) #f (max 0 (- (position-col cursor-in-buffer) 1)))]
         [lines-with-matches (find-matches-with-offset input lines initial-offset direction)]
         [lines-with-positions (align-matches-with-softwraps max-line-length lines-with-matches direction)])
        (unpack-matches input lines-with-positions))))

(define (flash-first-cursor-pos-on-screen)
  (if (and (list? (current-cursor)) (not (empty? (current-cursor))) (Position? (first (current-cursor))))
           (first (current-cursor))
           #f))

(define (flash-jump-label-alphabet)
  (let*
    ([configured-alphabet (get-config-option-value "jump-label-alphabet")])
    (if (or (not (string? configured-alphabet)) (not (> (string-length configured-alphabet) 0)))
        (map integer->char (range (char->integer #\a) (char->integer #\z)))
        (string->list configured-alphabet))))

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
    ([lines2 (reverse (take lines (+ 1 (position-row cursor-in-buffer))))])
    (if (empty? lines2)
        '()
        (let*
           ([trimmed-lines (append (list (substring (first lines2) 0 (position-col cursor-in-buffer))) (rest lines2))]
            [offset (floor (/ (position-col cursor-on-screen) line-width))]
            [max-lines (- max-lines (position-row cursor-on-screen))])
           (flash-pick-lines-backward-sub trimmed-lines offset max-lines line-width)))))

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
         [match-row (+ cursor-line-on-screen match-y)]
         [match-col (+ gutter input-len match-x)])
         (begin
           (frame-set-string! frame match-col match-row label label-style)
           ; INFO: this works extremely poorly with wrapped lines (soft-wraps)
           ; TODO: need to account for gutter + line width + line-wrap string (character?) and accumulate over all lines
           (map (lambda (x) (frame-set-string! frame (+ gutter match-x x) match-row (string (string-ref input x)) match-style)) (range input-len)))))
    matches)))

(define (flash-get-matches input direction max-line-width max-lines)
  (let*
    ([cursor-line-in-buffer (helix.static.get-current-line-number)]
     [cursor-column-in-buffer (helix.static.get-current-column-number)]
     [cursor-in-buffer (position cursor-line-in-buffer cursor-column-in-buffer)]
     [cursor-on-screen (flash-first-cursor-pos-on-screen)]
     [cursor-line-on-screen (position-row cursor-on-screen)]
     [max-line-width (- max-line-width (flash-get-gutter) 1)]
     [full-text (rope->string (editor->text (editor->doc-id (editor-focus))))]
     [lines (split-many full-text "\n")])
    (cond
      [(equal? 'backward direction)
       (flash-flat-matches
         input
         (flash-pick-lines-backward lines cursor-in-buffer cursor-on-screen max-lines max-line-width)
         max-line-width
         cursor-in-buffer
         cursor-on-screen
         direction)]
      [(equal? 'forward direction)
       (flash-flat-matches
         input
         (flash-pick-lines-forward lines cursor-in-buffer cursor-on-screen max-lines max-line-width)
         max-line-width
         cursor-in-buffer
         cursor-on-screen
         direction)]
      [else
        (append
          (flash-flat-matches
            input
            (flash-pick-lines-backward lines cursor-in-buffer cursor-on-screen max-lines max-line-width)
            max-line-width
            cursor-in-buffer
            cursor-on-screen
            'backward)
          (flash-flat-matches
            input
            (flash-pick-lines-forward lines cursor-in-buffer cursor-on-screen max-lines max-line-width)
            max-line-width
            cursor-in-buffer
            cursor-on-screen
            'forward))])))

(define (flash-render state rect frame)
  (if (> (string-length (hash-ref *flash-state* 'input)) 0)
    (let*
      ([direction (hash-ref *flash-state* 'direction)]
       [input (hash-ref *flash-state* 'input)]
       [max-line-width (area-width rect)]
       [max-lines (area-height rect)]
       [matches1 (flash-get-matches input direction max-line-width max-lines)]
       [matches (assign-labels matches1 (flash-jump-label-alphabet))])
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
        [found-match (find-first (lambda (m) (char=? key-char (hash-ref m 'label))) matches)]
        [should-extend (hash-ref *flash-state* 'extend)])
       (if found-match
          (begin
            (set-status! "")
            (helix.commands.goto-line (+ 1 (helix.static.get-current-line-number) (hash-ref found-match 'line-idx)) should-extend)
            (helix.commands.goto-column (+ (max 0 (- (string-length (hash-ref *flash-state* 'input)) 1)) (hash-ref found-match 'char-idx)) should-extend)
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
    (set! *flash-state*
      (~>
        (default-flash-state)
        (hash-insert 'direction 'everywhere)
        (hash-insert 'extend (equal? (editor-mode) "select"))))
    (set-status! "flash:")
    (flash-init)))

;;@doc
; Prefix search backward and jump
(define (flash-backward)
  (begin
    (set! *flash-state*
      (~>
        (default-flash-state)
        (hash-insert 'direction 'backward)
        (hash-insert 'extend (equal? (editor-mode) "select"))))
    (set-status! "flash [backward]:")
    (flash-init)))

;;@doc
; Prefix search forward and jump
(define (flash-forward)
  (begin
    (set! *flash-state*
      (~>
        (default-flash-state)
        (hash-insert 'direction 'forward)
        (hash-insert 'extend (equal? (editor-mode) "select"))))
    (set-status! "flash [forward]:")
    (flash-init)))

(define (flash-extend)
  (begin
    (set! *flash-state*
      (~>
        (default-flash-state)
        (hash-insert 'direction 'everywhere)
        (hash-insert 'extend #t)))
    (set-status! "flash [forward] [extend]:")
    (flash-init)))

(define (flash-extend-forward)
  (begin
    (set! *flash-state*
      (~>
        (default-flash-state)
        (hash-insert 'direction 'forward)
        (hash-insert 'extend #t)))
    (set-status! "flash [forward] [extend]:")
    (flash-init)))

(define (flash-extend-backward)
  (begin
    (set! *flash-state*
      (~>
        (default-flash-state)
        (hash-insert 'direction 'backward)
        (hash-insert 'extend #t)))
    (set-status! "flash [backward] [extend]:")
    (flash-init)))

(provide flash
         flash-backward
         flash-forward
         flash-extend
         flash-extend-backward
         flash-extend-forward)

