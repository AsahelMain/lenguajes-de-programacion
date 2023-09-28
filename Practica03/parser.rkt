#lang plai

(require "grammars.rkt")

(define (parse s-exp)
  (cond
    [(number? s-exp) (num s-exp)]
    [(symbol? s-exp) (id s-exp)]
    [(boolean? s-exp) (bool s-exp)]
    [(string? s-exp) (string s-exp)]
    [(list? s-exp)
     (let [(head (car s-exp))]
       (case head
         [(sub1 add1 not zero? num? str? bool? str-length sqrt) (if (= (length (cdr s-exp)) 1) (op (eval head) (map parse(cdr s-exp))) (error 'parse (string-append "La operacion " (symbol->string head) " espera 1 argumento. Numero de argumentos recibidos: " (number->string (length (cdr s-exp))) ".") ))]
         [(modulo expt) (if (= (length (cdr s-exp)) 2) (op (eval head) (map parse(cdr s-exp))) (error 'parse (string-append "La operacion " (symbol->string head) " espera 2 argumentos. Numero de argumentos recibidos: " (number->string (length (cdr s-exp))) ".") ))]
         [(+ - * / min max = < > <= >= anD oR) (if (> (length (cdr s-exp)) 0) (op (eval head) (map parse(cdr s-exp))) (error 'parse (string-append "La operacion " (symbol->string head) " espera mas de 0 argumentos. Numero de argumentos recibidos: " (number->string (length (cdr s-exp))) ".") ))]
         [(with) (with (map parse-binding(bindingList (second s-exp) '())) (parse (third s-exp)))]))]))

(define (parse-binding ls)
  (case (length ls)
    [(2) (binding (parse (first ls)) (parse (second ls)))]
    [else error 'parse "La estructura del binding es incorrecta."]))

(define (bindingList ls acc)
  (if (empty? ls)
      (empty)
      (let([head (first ls)])
        (if (member (car head) acc)
            (error 'parse (string-append "El identificador " (symbol->string (car head)) "esta declarado mas de una vez."))
            (cons (list-to-binding head) (bindingList (car ls) (cons (car head) (acc))))))))

(define (list-to-binding ls)
  binding (first ls) (second ls))