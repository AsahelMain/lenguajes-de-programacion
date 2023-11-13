#|@author{Arturo González Peñaloza 319091193}
@author{Asahel Said Main Cerezo 319260658}
@author{Emilio Arsenio Raudry Rico 318289276}
|#

#lang plai

(require "grammars.rkt")

#| Función que recibe una expresión RCFSBAE y la parsea
realizando su ASA

;; s-expr -> RCFSBAE |#
(define (parse s-exp)
  (cond
    [(number? s-exp) (num s-exp)]
    [(symbol? s-exp) (id s-exp)]
    [(boolean? s-exp) (bool s-exp)]
    [(string? s-exp) (strinG s-exp)]
    [(list? s-exp)
     (let [(head (car s-exp))]
       (case head
         [(sub1 add1 not zero? num? str? bool? str-length sqrt)
          (if (= (length (cdr s-exp)) 1)
              (op (translate head) (map parse(cdr s-exp)))
              (error 'parse (string-append "La operación " (symbol->string head) " debe ser ejecutada con 1 argumentos.")))]
         [(modulo expt)
          (if (= (length (cdr s-exp)) 2)
              (op (translate head) (map parse(cdr s-exp)))
              (error 'parse (string-append "La operación " (symbol->string head) " debe de ser ejecutada con 2 argumentos.")))]
         [(+ - * / min max = < > <= >= and or)
          (if (> (length (cdr s-exp)) 0)
              (op (translate head) (map parse(cdr s-exp)))
              (error 'parse (string-append "La operación " (symbol->string head) " debe ser ejecutada con mas de 0 argumentos." )))]
         [(fun) (fun (id-list (second s-exp) '()) (parse (third s-exp)))]
         [(if) (case (length (cdr s-exp))
                  [(0) (error 'parse "parse: Falta la if-condition.")]
                  [(1) (error 'parse "parse: Falta then-expression.")]
                  [(2) (error 'parse "parse: Falta la else-expression.")]
                  [else (iF (parse (second s-exp)) (parse (third s-exp)) (parse (fourth s-exp)) )])]
         [(rec) (rec (map list-to-binding (second s-exp)) (parse (third s-exp)))]
         [else (app (parse head) (map parse (cdr s-exp)))]
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

#| Función encargada de parsear una lista de
 asignacion (Binding)

list-to-binding: list -> ASA|#
(define (list-to-binding ls)
  (case (length ls)
    [(2) (binding (first ls) (parse (second ls)))]
    [else error 'parse "Estructura incorrecta de Binding."]
  ))

#| Función encargada de verificar que no haya
identificadores repetidos dentro la lista de
simbolos
id-list: list list -> list|#
(define (id-list ls acc)
  (if (empty? ls)
      (reverse acc)
      (if (member (first ls) (cdr ls))
          (error 'parse (string-append "Parámetro '" (symbol->string (first ls)) " definido dos veces."))
          (id-list (cdr ls) (cons (first ls) acc))
          ))
  )

#| Implementación propia de and
anD: args->bool|#
(define (anD . args)
  (foldl (lambda (a b) (and a b)) #t args))

#| Implementación propia de or
oR: args->bool|#
(define (oR . args)
  (foldl (lambda (a b) (or a b)) #f args))
