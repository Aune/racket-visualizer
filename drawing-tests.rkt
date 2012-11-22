#lang racket


(require racket/gui)

(define window (new frame% 
                    [label "Example"]
                    [width 400]
                    [height 400]))

(define main (new horizontal-panel% 
                    [parent window]
                    [border 10]
                    [vert-margin 0]
                    [horiz-margin 0]
                    [min-width 400]
                    [min-height 400]
                    [stretchable-height #f]
                    [stretchable-width #f]))

(define global (new horizontal-panel% 
                    [parent main]
                    [border 10]
                    [vert-margin 0]
                    [horiz-margin 0]
                    [min-width 200]
                    [min-height 400]
                    [stretchable-height #f]
                    [stretchable-width #f]))

(define local-cxt (new horizontal-panel% 
                    [parent main]
                    [border 10]
                    [vert-margin 0]
                    [horiz-margin 0]
                    [min-width 200]
                    [min-height 400]
                    [stretchable-height #f]
                    [stretchable-width #f]))


(define text1 (new message% [parent global] 
                   [label "This is a \n line break!"]))

(define text2 (new message% [parent local-cxt] 
                   [label "This is another \n line break!"]))



(send window show #t)
