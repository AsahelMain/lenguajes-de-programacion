#lang plai
#| Ejercicio 6 |#
(require "grammars.rkt")

#| Función que recibe una expresión TCFWSBAE y la parsea
realizando su ASA

;; parse :: s-exp -> TCFWSBAE|#
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
         [(with) (with (bindingList (second s-exp) '()) (parse (third s-exp)))]
         [(with*) (with* (map list-to-binding (second s-exp)) (parse (third s-exp)))]
         [(fun) (fun (param-list (second s-exp) '()) (parse-type (fourth s-exp)) (parse (fifth s-exp)))]
         [(if) (case (length (cdr s-exp))
                  [(0) (error 'parse "parse: Falta la if-condition.")]
                  [(1) (error 'parse "parse: Falta then-expression.")]
                  [(2) (error 'parse "parse: Falta la else-expression.")]
                  [else (iF (parse (second s-exp)) (parse (third s-exp)) (parse (fourth s-exp)) )])]
         [(cond) (if (<= (length (cdr s-exp)) 1) (error 'parse "parse: La expresión conds debe contar con 1 o mas condiciones y una expresión else.")
                     (let [(elSE (last s-exp))]
                     (conD (map list-to-condition (take (cdr s-exp) (sub1 (length (cdr s-exp)))))
                            (parse (second elSE)))))]
         [else (app (parse head) (map parse (cdr s-exp)))]
         ))]))

(define (parse-type ty)
    (case ty
      [(num) (numberT)]
      [(bool) (booleanT)]
      [(strinG) (stringT)]
      [(list?) (funT (map parse-type (filter deleteArrow ty)))]
    )
)

(define (deleteArrow ty) (if (equal? ty '->) #f #t))

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

#| Función encargada de parsear una lista de
 asignacion (Binding)

list-to-binding: list -> ASA|#
(define (list-to-binding ls)
  (case (length ls)
    [(4) (binding (first ls) (parse-type (third ls)) (parse(fourth ls)))]
    [else error 'parse "Estructura incorrecta de Binding."]
  ))

#| Función encargada de verificar que no haya
identificadores repetidos dentro la lista de
asignaciones para poder parsearla

bindingList: list list -> ASA|#
(define (param-list ls acc)
  (if (empty? ls)
      empty
      (let([head (first ls)])
        (if (member (car head) acc)
            (error 'parse (string-append "El identificador " (symbol->string (car head)) " está declarado más de una vez."))
            (cons (list-to-param head) (param-list (cdr ls) (cons (car head) acc)))))))

#| Función encargada de parsear una lista de
 asignacion (Binding)

list-to-binding: list -> ASA|#
(define (list-to-param ls)
  (case (length ls)
    [(3) (param (first ls) (parse-type (third ls)))]
    [else error 'parse "Estructura incorrecta de Binding para fun."]
  ))

#| Función envargada de parsear correctamente una Condition
list-to-condition: list -> condition
|#
(define (list-to-condition ls)
  (case (length ls)
    [(2) (condition (parse (first ls)) (parse (second ls)))]
    [else error 'parse "Estructura incorrecta de Condition."]
    ))

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

(define (anD . args)
  (foldl (lambda (a b) (and a b)) #t args))

(define (oR . args)
  (foldl (lambda (a b) (or a b)) #f args))
