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
              (match (car assigns)
                [(binding id val)
                  (if (symbol=? id sub-id)
                      (with (list (binding id (subst sub-id value val))) body)
                      (with (list (binding id (subst sub-id value val))) (subst sub-id value body)))])]
      
        [with* (assigns body)
              (match (car assigns)
                [(binding id val)
                  (if (symbol=? id sub-id)
                      (with (list (binding id (subst sub-id value val))) body)
                      (with (list (binding id (subst sub-id value val))) (subst sub-id value body)))])]))
      



      