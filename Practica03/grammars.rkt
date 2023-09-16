#lang plai

(define-type Binding
  [binding (id symbol?) (value WAE?)])

(define-type WAE
  [num (n number?)]
  [id (i symbol?)]
  [bool (b boolean?)]
  [strinG (s string?)]
  [op (f procedure?) (args (listof WAE?))]
  [with (assigns (listof Binding?)) (body WAE?)]
  [with* (assigns (listof Binding?)) (body WAE?)])

(define (anD . args)
  (foldl (lambda (a b) (and a b)) #t args))

(define (oR . args)
  (foldl (lambda (a b) (or a b)) #f args))
