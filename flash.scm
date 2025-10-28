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

(define (flash-initial)
  (let* ([cursor-line
           (if
             (Position? cursor-position)
             (position-row cursor-position)
             0)]
         [text (rope->string (rope->line editor->text cursor-line))])
    (flash-initial text)))

(define (flash-render state rect frame)
  (let*
    ([cursor-pos 
       (if (and (list? (current-cursor)) (not (empty? (current-cursor))) (Position? (first (current-cursor))))
           (first (current-cursor))
           #f)]
     [frame-rect
       (if (Rect? (editor-focused-buffer-area))
         (editor-focused-buffer-area)
         ;(buffer-area frame)
         #f)]
     [input (hash-ref state 'input)])
  (begin
    (set-warning! (to-string "flash:" input))
    (frame-set-string! frame (+ (area-y rect) (+ 1 (position-col cursor-pos))) (+ (area-x rect) (position-row cursor-pos)) "?" (style-with-bold (theme-scope "ui.cursor.match")))
    )))

(define (flash-handle-event state event)
  (when (key-event? event)
    (begin
      (cond
        ((key-event-escape? event)
         (pop-last-component-by-name! "flash"))
        ((key-event-backspace? event)
         (let*
           ([input (hash-ref state 'input)]
            [input-len (string-length input)]
            [key-str (to-string (key-event-char event))])
           (when (> 0 input-len)
             (hash-insert state 'input (substring input 0 (- 1 input-len))))))
        ((key-event-char event)
          (hash-insert state 'input (string-append (hash-ref state 'input) (to-string (key-event-char event))))))
    event-result/consume)))

(define (flash)
  (push-component! (new-component! "flash" (hash 'input "") flash-render (hash "handle_event" flash-handle-event))))

(provide flash)

