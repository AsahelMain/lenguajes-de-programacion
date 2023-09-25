#lang plai

(require "grammars.rkt")
;(require "parser.rkt")

;(define (interp expr))


#|
  subst: symbol WAE WAE -> WAE
|#
(define (subst sub-id value expr)
    (type-case WAE expr
        [num (n) expr]
        [id (i) (if (symbol=? i sub-id) value expr)]
        [bool (b) expr]
        [str (s) expr]
        [op (f args) (op f (map (lambda (expr) (subst sub-id value expr)) args))]
        [with (assigns body)
              (cond
                [(estaEnBindings? sub-id assigns) (with (substBindings sub-id value assigns) body)]
                [else (with (substBindings sub-id value assigns) (subst sub-id value body))])]
      
        [with* (assigns body)
              (cond
                [(estaEnBindings? sub-id assigns) (with (substBindings sub-id value assigns) body)]
                [else (with (substBindings sub-id value assigns) (subst sub-id value body))])]))

(define (estaEnBindings? sub-id assigns)
  (ormap (lambda (binding)
           (if (symbol=? (binding-id binding) sub-id)
               #t
               #f))
         assigns))

(define (crear-binding id value)
  (binding id value))

(define (substBindings sub-id value assigns)
  (map (lambda (binding)
         (cond
           [(binding? binding)
            (let ([id (binding-id binding)]
                  [val (binding-value binding)])
              (crear-binding id (subst sub-id value val)))] 
           [else binding]))
       assigns))
  



      