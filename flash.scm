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

;; ----------------------------------------

(define (default-flash-state) (hash 'input "" 'start-pos (position 0 0) 'gutter 0 'cursor-to-be #f))
(define *flash-state* (default-flash-state))

(define (flash-update-status)
  (let* ([input (hash-ref *flash-state* 'input)])
    (set-status! (to-string "flash:" input))))

(define (map-matches idx input line)
  (map (lambda (ms) (append (list ms) (list idx))) (find-matches input line)))

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
  (map string (map integer->char (range (char->integer #\a) (char->integer #\z)))))

(define (flash-render state rect frame)
  (let*
    ([cursor-on-screen (flash-first-cursor-pos-on-screen)]
     [cursor-in-buffer (position (helix.static.get-current-line-number) (helix.static.get-current-column-number))]
     [cursor-line-on-screen (position-row cursor-on-screen)]
     [cursor-line-in-buffer (position-row cursor-in-buffer)]
     [cursor-column-in-buffer (position-col cursor-in-buffer)]
     [cursor-column-on-screen (position-col cursor-on-screen)]
     [gutter (hash-ref *flash-state* 'gutter)]
     [lines-on-screen (area-height rect)]
     [alphabet (flash-jump-label-alphabet)]
     [full-text (editor->text (editor->doc-id (editor-focus)))]
     [lines (map (lambda (n) (rope->string (rope->line full-text n))) (range cursor-line-in-buffer (+ 1 cursor-line-in-buffer (- lines-on-screen cursor-line-on-screen))))]
     [input (hash-ref *flash-state* 'input)]
     [style (style-with-bold (theme-scope "ui.cursor.match"))])
    (if (> (string-length input) 0)
        (let*
          ([matches (matches-and-labels input lines alphabet)])
          (begin
           ; (map (lambda (a) (frame-set-string! frame (first a) (+ cursor-line-on-screen (second a)) (+ gutter (third a)) style)) matches) ; TODO: throws panic
           ; (frame-set-string! frame (+ 1 cursor-column-on-screen) 1 input style)
           ; (flash-update-status)
           (set-status! (to-string "flash: " input " ; gutter: " gutter "; cursor: " cursor-column-on-screen "/" cursor-column-in-buffer "->" (first (first matches)) "/" (second (first matches)) "/" (third (first matches))))
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
     (set! *flash-state* (hash-insert *flash-state* 'cursor-to-be (hash-ref *flash-state* 'start-pos)))
     event-result/close]
    [(key-event-backspace? event)
     (flash-remove-last-input-char)
     (flash-update-status)
     event-result/consume]
    [(char? key-char)
      (flash-append-input-char key-char)
      (flash-update-status)
      event-result/consume]
    [else
      (enqueue-thread-local-callback (lambda () void))
      event-result/ignore]))

(define (flash-handle-cursor-event state event)
  (let*
    ([pos (hash-ref *flash-state* 'cursor-to-be)])
    (begin
      ; (set! *flash-state* (hash-insert *flash-state* 'cursor-to-be #f))
        pos)))

(define (flash)
  (begin
    (set! *flash-state* (hash-insert (default-flash-state) 'start-pos (flash-first-cursor-pos-on-screen)))
    (helix.static.goto_line_start)
    (set! *flash-state* (hash-insert *flash-state* 'gutter (position-col (flash-first-cursor-pos-on-screen))))
    (set! *flash-state* (hash-insert *flash-state* 'cursor-to-be (hash-ref *flash-state* 'start-pos))) ; TODO: this creates a second cursor, but does not actually move the original cursor
    (push-component!
      (new-component!
        "flash"
        *flash-state*
        flash-render
        (hash
          "handle_event" flash-handle-event
          "cursor" flash-handle-cursor-event)))))

(provide flash)

