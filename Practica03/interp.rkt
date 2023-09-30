#lang plai

(require "grammars.rkt")
(require "parser.rkt")

#| Ejercicio 4 |#
#| Función que recibe una expresión WAE y la interpreta.
   interp WAE -> number | boolean | string
|#
(define (interp expr)
  (type-case WAE expr
             [id (i) (error 'interp "Variable libre: '~a" i)]
             [num (n) n]
             [bool (b) b]
             [strinG (s) s]
             [op (f args)
                 (let ([args-interpretados (map interp args)])                   
                   ;; Se revisa que los tipos de los argumentos dados sean validos.
                   ;; En caso de no serlo se imprime el procedimiento con el tipo de argumento
                   ;; esperado y el recibido.
                   ;; Se decidió manejar los errores de esta forma debido a que
                   ;; Racket lo hace de una forma similar.
                   (when (verifica-argumentos f args-interpretados)
                       (apply f args-interpretados)))]
             [with (assigns body)
                   ;; Se sustituye cada id en el cuerpo y se interpreta.
                   (interp (foldl (lambda (binding acc)
                                    (subst (binding-id binding) (binding-value binding) acc))
                                  body
                                  assigns))]
             ;; Se interpreta la expresión transformada a withs anidados.
             [with* (assigns body) (interp (with*->with expr))]))

#|Función auxiliar de interp que verifica que los argumentos dados a
  un procedimiento sean válidos, en caso de no serlos muestra un error.

  verifica-argumentos: procedure (listof any) -> boolean
|#
(define (verifica-argumentos f args)
  (let ([ops-con-num '(+ - / * min max expt sqrt sub1 add1 < > <= >= = zero?)]
        [ops-con-int '(modulo )]
        [ops-con-bool '(not anD oR)]
        [ops-con-str '(string-length string?)])
    (cond 
      [(ormap (lambda (x) (eq? (eval x) f)) ops-con-num)
       (if (andmap number? args)
           #t
           (error 'interp "~a: error: violación de contrato\nesperado: num\ndado: ~a"
                  f (findf (lambda (x) (not (number? x))) args)))]
      [(ormap (lambda (x) (eq? (eval x) f)) ops-con-int) (if (andmap integer? args)
                                                            #t
                                                            (error 'interp "~a: error: violación de contrato\nesperado: entero\ndado: ~a"
                                                                   f (findf (lambda (x) (not (integer? x))) args)))]
      [(ormap (lambda (x) (eq? (eval x) f)) ops-con-bool) (if (andmap boolean? args)
                                                            #t
                                                            (error 'interp "~a: error: violación de contrato\nesperado: bool\ndado: ~a"
                                                                   f (findf (lambda (x) (not (boolean? x))) args)))]
      [(ormap (lambda (x) (eq? (eval x) f)) ops-con-str) (if (andmap string? args)
                                                            #t
                                                            (error 'interp "~a: error: violación de contrato\nesperado: str\ndado: ~a"
                                                                   f (findf (lambda (x) (not (string? x))) args)))]
      [else #t])))


#|Función auxiliar de interp y subst que transforma una expresión WAE with* a expresiones
  with anidadas. Se usa para simplificar los procedimientos que involucran a with*.

  with*->with: WAE -> WAE
|#
(define (with*->with expr)
  (let ([bindings (with*-assigns expr)]
        [body (with*-body expr)])
    (with (list (first bindings))
          (if (empty? (rest bindings))
              body
              (with*->with (with* (rest bindings) body))))))

#| Ejercicio 4 |#
#| Función que recibe un identificador, un valor
   y una expresión WAE, y sustituye las apariciones
   del identificador por el valor dentro de la
   expresión

   subst: symbol WAE WAE -> WAE
|#
(define (subst sub-id value expr)
    (type-case WAE expr
        [num (n) expr]
        [id (i) (if (symbol=? i sub-id) value expr)]
        [bool (b) expr]
        [strinG (s) expr]
        [op (f args) (op f (map (lambda (expr) (subst sub-id value expr)) args))]
        [with (assigns body)
              (cond
                [(estaEnBindings? sub-id assigns) (with (substBindings sub-id value assigns) body)]
                [else (with (substBindings sub-id value assigns) (subst sub-id value body))])]
      
        [with* (assigns body) (subst sub-id value (with*->with expr))]))


#|Función auxiliar de subst
  Recibe un identificador y una lista de
  objetos de tipo Binding. Regresa true
  si el identificador se encuentra presente
  en el atributo id de al menos un Binding
  Regresa false en caso contrario.

  estaEnBindings?: symbol listOfBinding -> boolean

|#
(define (estaEnBindings? sub-id assigns)
  (ormap (lambda (binding)
           (if (symbol=? (binding-id binding) sub-id)
               #t
               #f))
         assigns))

#| Función auxiliar de substBindings
   Permite crear un objeto de tipo binding
   dado un identificador y una expresión WAE

   crear-binding: symbol WAE -> Binding
|#
(define (crear-binding id value)
  (binding id value))

#|Función auxiliar de subst
  Recibe un identificador, un valor WAE y
  una lista de objetos de tipo Binding.
  Aplica la función subst al atributo value
  de cada Binding, y regresa la lista de
  Bindings con los valores sustituidos

  substBindings: symbol WAE listOfBindings -> listOfBindings
|#
(define (substBindings sub-id value assigns)
  (map (lambda (binding)
         (cond
           [(binding? binding)
            (let ([id (binding-id binding)]
                  [val (binding-value binding)])
              (crear-binding id (subst sub-id value val)))]
           [else binding]))
       assigns))
