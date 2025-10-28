(require "helix/editor.scm")
(require "helix/commands.scm")
(require "helix/static.scm")
(require "helix/components.scm")
(require "helix/misc.scm")
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

(define *flash-state* (hash 'input ""))

(define (flash-initial)
  (let* ([cursor-line
           (if
             (Position? cursor-position)
             (position-row cursor-position)
             0)]
         [text (rope->string (rope->line editor->text cursor-line))])
    (flash-initial text)))

(define (flash-update-status)
  (let* ([input (hash-ref *flash-state* 'input)])
    (set-status! (to-string "flash:" input))))

(define (flash-first-cursor-pos)
  (if (and (list? (current-cursor)) (not (empty? (current-cursor))) (Position? (first (current-cursor))))
           (first (current-cursor))
           #f))

(define (flash-frame-rect)
  (if (Rect? (editor-focused-buffer-area))
         (editor-focused-buffer-area)
         ;(buffer-area frame)
         #f))

(define (flash-render state rect frame)
  (let*
    ([cursor-pos (flash-first-cursor-pos)]
     [frame-rect (flash-frame-rect)]
     [input (hash-ref *flash-state* 'input)]
     [x (+ (area-y rect) 1 (position-col cursor-pos))]
     [y (+ (area-x rect) (position-row cursor-pos))]
     [style (style-with-bold (theme-scope "ui.cursor.match"))])
    (flash-update-status)
    (frame-set-string! frame x y "?" style)))

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

