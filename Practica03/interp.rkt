#lang plai

(require "grammars.rkt")
;(require "parser.rkt")

(define (interp expr)
  (type-case WAE expr
           [id (i) (error 'interp "Variable libre")]
           [num (n) n]
           [bool (b) b]
           [str (s) s]
           [op (f args)
               (with-handlers ([exn:fail?
                                (lambda (exn)
                                  (exn-message exn))])
                 (let ([args-interpretados (map interp args)])
                   (with-handlers ([exn:fail?
                                    (lambda (exn)
                                      (error 'interp  "error: violación de contrato\n~a" (exn-message exn)))])
                     (apply f args-interpretados))   ))
               
               ]
           [with (assigns body)
                 (interp (foldl (lambda (binding acc)
                                  (subst (binding-id binding) (binding-value binding) acc))
                                body
                                assigns))]
           [with* (assigns body) (interp (with*->with expr))]))

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
        [str (s) expr]
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
