#lang racket


(require "interpreter.rkt")
(require racket/gui)



(define main-window (new frame% [label "Arnes awesome machine"]
                                [width 1040]
                                [height 480]))

(define global-cxt (new canvas% [parent main-window]
                        [paint-callback
                         (lambda (canvas dc)
                           (send dc set-scale 1 1)
                           (send dc set-text-foreground "black")
                           (send dc draw-text (gen-text global-context) 0 0))]))

(define (gen-text frame)
  (let* ((bindings (frame-bindings frame))
         (lines (map (lambda (pair) (format "~s = ~s~n")) bindings)))
    (string-append lines)))