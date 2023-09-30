#lang plai

(define-type Binding
  [binding (id symbol?) (value WAE?)])

#| Ejercicio 1 |#
#| Gramática WAE extendida.
|#
(define-type WAE
  [num (n number?)]
  [id (i symbol?)]
  [bool (b boolean?)]
  [strinG (s string?)]
  ;; Acepta cualquier procedimiento como operación.
  [op (f procedure?) (args (listof WAE?))]
  [with (assigns (listof Binding?)) (body WAE?)]
  [with* (assigns (listof Binding?)) (body WAE?)])

#| Ejercicio 2 |#
#| Función que recibe n argumentos y aplica al operador lógico
   and sobre todos los argumentos.
   anD: any any ... any -> boolean
|#
(define (anD . args)
  (foldl (lambda (a b) (and a b)) #t args))

#| Ejercicio 2 |#
#| Función que recibe n argumentos y aplica al operador lógico
   or sobre todos los argumentos.
   oR: any any ... any -> boolean
|#
(define (oR . args)
  (foldl (lambda (a b) (or a b)) #f args))
