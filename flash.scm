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

(define (default-flash-state) (hash 'input ""))
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

(define (flash-max-line-on-screen rect)
  (let*
    ([cursor-line-on-screen (position-row (flash-first-cursor-pos-on-screen))]
     [cursor-line-in-buffer (helix.static.get-current-line-number)]
     [lines-on-screen (area-height rect)])
    (- lines-on-screen (- cursor-line-in-buffer cursor-line-on-screen))))

(define (flash-jump-label-alphabet)
  (map string (map integer->char (range (char->integer #\a) (char->integer #\z)))))

(define (flash-render state rect frame)
  (let*
    ([cursor-line (helix.static.get-current-line-number)]
     [max-line (flash-max-line-on-screen rect)]
     [cursor-line-on-screen (position-row (flash-first-cursor-pos-on-screen))]
     [cursor-line-in-buffer (helix.static.get-current-line-number)]
     [lines-on-screen (area-height rect)]
     [alphabet (flash-jump-label-alphabet)]
     [full-text (editor->text (editor->doc-id (editor-focus)))]
     [lines (map (lambda (n) (rope->string (rope->line full-text n))) (range cursor-line (+ 1 cursor-line (- lines-on-screen cursor-line-on-screen))))]
     [input (hash-ref *flash-state* 'input)]
     [style (style-with-bold (theme-scope "ui.cursor.match"))])
    (if (> (string-length input) 0)
        (let*
          ([matches (matches-and-labels input lines alphabet)])
          (begin
           (map (lambda (a) (begin (frame-set-string! frame (first a) (+ cursor-line-on-screen (second a)) (third a) style) #t)) matches)
           (flash-update-status))))))

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
     (set! *flash-state* (default-flash-state))
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

(define (flash-handle-cursor-event state event) #f)

(define (flash)
  (push-component!
    (new-component!
      "flash"
      *flash-state*
      flash-render
      (hash
        "handle_event" flash-handle-event
        "cursor" flash-handle-cursor-event))))

(provide flash)

