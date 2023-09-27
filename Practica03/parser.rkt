#lang plai

(require "grammars.rkt")

(define (parse s-exp)
  (cond
    [(number? s-exp) (num s-exp)]
    [(symbol? s-exp) (id s-exp)]
    [(boolean? s-exp) (bool s-exp)]
    [(string? s-exp) (strinG s-exp)]
    [(Binding? s-exp) (binding parse(s-exp))]
    [(list? s-exp)
     (let [head (car s-exp)]
       (case head
         [(sub1 add1 not zero? num? str? bool? str-length) (if (= (length (cdr s-exp)) 1) (op head (list parse(cdr s-exp))) (error 'parse (string-append "La operacion " (symbol->string head) "espera 1 argumento. Numero de argumentos recibidos: " (number->string (length (cdr s-exp))) ".") ))]
         [(modulo expt) (if (= (length (cdr s-exp)) 2) (op head (list (map parse(cdr s-exp)))) (error 'parse (string-append "La operacion " (symbol->string head) "espera 2 argumento. Numero de argumentos recibidos: " (number->string (length (cdr s-exp))) ".") ))]
         [(+ - * / min max sqrt < > <= >= anD oR) (if (> (length (cdr s-exp)) 0) (op head (list (map parse(cdr s-exp)))) (error 'parse (string-append "La operacion " (symbol->string head) "espera mas de 0 argumentos. Numero de argumentos recibidos: " (number->string (length (cdr s-exp))) ".") ))]
         [(with) (let [repetido (symRepetido (flatten (second s-exp)))]
                   (case repetido
                     [(null?) (with (parse(second s-exp)) (parse(third s-exp)))]
                     [else (error 'parse (string-append "El identificador " (symbol->string repetido) " esta declarado mas de una vez."))]))]
         [(with*) (let [repetido (symRepetido (flatten (second s-exp)))]
                   (case repetido
                     [(null?) (with (parse(second s-exp)) (parse(third s-exp)))]
                     [else (error 'parse (string-append "El identificador " (symbol->string repetido) " esta declarado mas de una vez."))]))]
         [(Binding?) (list parse(head) parse(cdr-ls))]
         ))
     ]))

(define (symRepetido ls)
  (case (car ls)
    [(symbol?) (if (null? (cdr ls)) null (case (member (car ls) (cdr ls))
                                           [(#f) (symRepetido (cdr ls))]
                                           [else car ls]))]
    [else (if (null? (cdr ls)) null (symRepetido (cdr ls)))]))

