#lang plai

(require "grammars.rkt")
(require "parser.rkt")

(define (anD . args)
  (foldl (lambda (a b) (and a b)) #t args))

(define (oR . args)
  (foldl (lambda (a b) (or a b)) #t args))
