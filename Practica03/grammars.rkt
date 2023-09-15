#lang plai

(define-type Binding
  [binding (id symbol?) (value WAE?)])

(define-type WAE
  [id (i symbol?)]
  [num (n number?)]
  [with (assigns (listof Binding?)) (body WAE?)]
  [bool (b boolean?)]
  [strinG (s string?)]
  [with* (assigns (listof Binding?)) (body WAE?)])

(define (anD . args)
  (foldl (lambda (a b) (and a b)) #t args))

(define (oR . args)
  (foldl (lambda (a b) (or a b)) #f args))
