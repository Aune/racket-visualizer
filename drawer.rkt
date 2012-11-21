#lang racket


(require "interpreter.rkt")
(require racket/gui)


; Dummy struct functions
(define (frame-bindings a) '((plus . isplus)(variable . 15)(list . (1 2 3))))
;(define global-context 1)

(define main-window (new frame% [label "Arnes awesome machine"]
                                [width 1040]
                                [height 480]))

(define global-cxt (new canvas% [parent main-window]
                        [paint-callback
                         (lambda (canvas dc)
                           (send dc set-scale 1 1)
                           (send dc set-text-foreground "black")
                           (draw-text-list dc (gen-text global-context) 0 0))]))

(define (gen-text frame)
  (let* ((bindings (frame-bindings frame))
         (lines (map (lambda (pair) (format "~s = ~s ~n" (car pair) (cdr pair))) bindings)))
    lines))

(define (draw-text-list dc list x0 y0)
  (if (not (null? list))
      (begin
        (send dc draw-text (car list) x0 y0)
        (draw-text-list dc (cdr list) x0 (+ y0 15)))
      (void)))

(send main-window show #t)