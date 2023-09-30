#lang plai
#| Ejercicio 3 |#
(require "grammars.rkt")

#| Función que recibe una expresión WAE y la parsea
realizando su ASA

parse: WAE -> ASA|#
(define (parse s-exp)
  (cond
    [(number? s-exp) (num s-exp)]
    [(symbol? s-exp) (id s-exp)]
    [(boolean? s-exp) (bool s-exp)]
    [(string? s-exp) (strinG s-exp)]
    [(list? s-exp)
     (let [(head (car s-exp))]
       (case head
         [(sub1 add1 not zero? num? str? bool? str-length sqrt) (if (= (length (cdr s-exp)) 1) (op (eval (translate head)) (map parse(cdr s-exp))) (error 'parse (string-append "La operación " (symbol->string head) " debe ser ejecutada con 1 argumentos.")))]
         [(modulo expt) (if (= (length (cdr s-exp)) 2) (op (eval head) (map parse(cdr s-exp))) (error 'parse (string-append "La operación " (symbol->string head) " debe de ser ejecutada con 2 argumentos.")))]
         [(+ - * / min max = < > <= >= and or) (if (> (length (cdr s-exp)) 0) (op (eval (translate head)) (map parse(cdr s-exp))) (error 'parse (string-append "La operación " (symbol->string head) " debe ser ejecutada con mas de 0 argumentos." )))]
         [(with) (with (bindingList (second s-exp) '()) (parse (third s-exp)))]
         [(with*) (with* (map list-to-binding (second s-exp)) (parse (third s-exp)))]
))]))
#| Función encargada de verificar que no haya
identificadores repetidos dentro la lista de
asignaciones para poder parsearla

bindingList: list list -> ASA|#
(define (bindingList ls acc)
  (if (empty? ls)
      empty
      (let([head (first ls)])
        (if (member (car head) acc)
            (error 'parse (string-append "El identificador " (symbol->string (car head)) " está declarado más de una vez."))
            (cons (list-to-binding head) (bindingList (cdr ls) (cons (car head) acc)))))))

#| Traduce los operadores implementados por su equivalente en Racket

translate: procedure -> procedure|#
(define (translate op)
  (case op
    [(str-length) string-length]
    [(num?) number?]
    [(str?) string?]
    [(bool?) boolean?]
    [(and) anD]
    [(or) oR]
    [else op]
    ))

#| Función encargada de parsear una lista de
 asignacion (Binding)

list-to-binding: list -> ASA|#
(define (list-to-binding ls)
  (case (length ls)
    [(2) (binding (first ls) (parse (second ls)))]
    [else error 'parse "Estructura incorrecta de Binding."]
  ))
