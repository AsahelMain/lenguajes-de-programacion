#lang plai

(require "grammars.rkt")

;; parse :: s-exp -> CFWSBAE
(define (parse s-exp) (error '0))



(define (anD . args)
  (foldl (lambda (a b) (and a b)) #t args))

(define (oR . args)
  (foldl (lambda (a b) (or a b)) #f args))


#| Traduce los operadores implementados por su equivalente en Racket

translate: procedure -> procedure|#
(define (translate op)
  (case op
    [(+) +]
    [(-) -]
    [(-) -]
    [(/) /]
    [(*) *]
    [(modulo) modulo]
    [(min) min]
    [(max) max]
    [(expt) expt]
    [(sqrt) sqrt]
    [(sub1) sub1]
    [(add1) add1]
    [(<) <]
    [(>) >]
    [(<=) <=]
    [(>=) >=]
    [(=) =]
    [(not) not]
    [(and) anD]
    [(or) oR]
    [(zero?) zero?]
    [(num?) number?]
    [(str?) string?]
    [(bool?) boolean?]
    [(str-length) string-length]))