#lang racket

; TODO
; -implement a set! procedure.
;

(module interpreter racket
  (provide (except-out 
            (all-defined-out)
            )))


; Basic structures

(struct frame (parent [bindings #:mutable]))

(struct proc (args body context))

; Helper functions

(define (zip a b)
  (if (or (null? a) (null? b))
      null
      (cons (cons (car a) (car b)) (zip (cdr a) (cdr b)))))

; Primitive functions

(define primitives 
  (zip '(+ - = car cdr cons null? print)
       (list + - = car cdr (lambda (x y) (cons (evaluate x) (evaluate y)))  null? print)))


; Basic syntactic scoping management

(define global-context
  (frame false primitives))

(define call-stack
  
  (cons global-context '()))

(define (current-context)
  (car call-stack))

(define (push-context ctx)
  (set! call-stack (cons ctx call-stack)))

(define (pop-context)
  (set! call-stack (cdr call-stack)))

(define (bind name val)
  (let* ((ctx (current-context))
         (bindings (frame-bindings ctx)))
    (set-frame-bindings! ctx (cons (cons name val)
                                   bindings))))

; Basic functions

(define (search context name)
  (cond ((not context)
         (printf "Could not find ~s~n" name)
         (error "name not found"))
        ((dict-has-key? (frame-bindings context) name)
         (dict-ref (frame-bindings context) name))
        (else
         (search (frame-parent context) name))))

(define (run-proc proc args)
  (if (= (length args)
         (length (proc-args proc)))
      (let* ((bindings (zip (proc-args proc) args))
             (context (frame (proc-context proc) bindings))
             (ans '()))
        (begin
           (push-context context)
           (set! ans (evaluate (proc-body proc)))
           (pop-context)
           ans))
      (printf "Argument miss-match: ~s  =/=  ~s ~n" (proc-args proc) args)))
        
        

(define (evaluate expr)
  (cond ((symbol? expr)
         (search (current-context) expr))
        ((null? expr)
         null)
        ((list? expr)
         (let ((head (car expr))
               (tail (cdr expr)))
           (cond ((list? head)
                  (evaluate (cons (evaluate head) tail)))
                 ((proc? head)
                  (run-proc head tail))
                 ((procedure? head)
                  (apply head (map evaluate tail)))
                 ((eq? head 'lambda)
                  (proc (car tail) (cadr tail) (current-context)))
                 ((eq? head 'bind)
                  (bind (car tail) (evaluate (cadr tail))))
                 ((eq? head 'begin)
                  (for-each evaluate tail))
                 ((eq? head 'if)
                  (if (evaluate (car tail))
                      (evaluate (cadr tail))
                      (evaluate (caddr tail))))
                 ((symbol? head)
                  (evaluate (cons (evaluate head)
                                  (map evaluate tail))))
                 (else ;(printf "Non function: ~s~n" expr)
                       ;(error "Can not apply non procedure object")
                       (map evaluate expr)))))
        (else expr)))


;; Test functions
;(evaluate '(+ 3 2))
;(evaluate '(- 6 1))
;(evaluate '(if (= 1 2) 10 5))
;(evaluate '(if (= 1 1) 5 10))
;
;; Defines map and bind is to its name, then we test it
;(evaluate '(bind map (lambda (fun list) (if (null? list) () (cons (fun (car list)) (map fun (cdr list)))))))
;(evaluate '(bind inc (lambda (arg) (+ arg 1))))
;
;(evaluate '(map inc (1 2 3 4 5)))
;
;; Defines fold and test it
;(evaluate '(begin (bind fold (lambda (f x) (if (null? x) '() (if (null? (cdr x)) (car x) (f (car x) (fold f (cdr x)))))))
;                  (bind x (fold + (1 2 3 4 5)))
;                  (print x)))
;
;; Testing some object orientation and lexical scoping rules.
;(printf "~n~n")
;
;(evaluate '(bind obj ((lambda (hidden) (lambda (x) (+ x hidden))) 3)))
;(evaluate '(obj 3))
;
;(printf "~n")
;
;; Testing shadowing
;(evaluate '(bind nested ((lambda (x) ((lambda (x) (lambda (var) (+ x var))) 3)) 4)))
;(evaluate '(nested 1))
  

; Graphics /should be in drawer.rkt


(require racket/gui)


(define main-window (new frame% [label "Arnes awesome machine"]
                                [width 640]
                                [height 480]))

(define global-cxt (new message% 
                        [label "Global Context"]
                        [parent main-window]
                        ))

(define (gen-text dc frame)
  (let* ((bindings (frame-bindings frame))
         (lines (map (lambda (pair) (format "~s = ~s ~n" (car pair) (cdr pair))) bindings)))
    (for-each (lambda (str) (send dc set-label str)) lines)))

(gen-text global-cxt global-context)
(send main-window show #t)