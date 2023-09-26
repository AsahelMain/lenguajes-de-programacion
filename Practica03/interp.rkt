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


#|
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
      
        [with* (assigns body)
              (cond
                [(estaEnBindings? sub-id assigns) (with* (substBindings sub-id value assigns) body)]
                [else (with* (substBindings sub-id value assigns) (subst sub-id value body))])]))

(define (estaEnBindings? sub-id assigns)
  (ormap (lambda (binding)
           (if (symbol=? (binding-id binding) sub-id)
               #t
               #f))
         assigns))

(define (crear-binding id value)
  (binding id value))

(define (substBindings sub-id value assigns)
  (map (lambda (binding)
         (cond
           [(binding? binding)
            (let ([id (binding-id binding)]
                  [val (binding-value binding)])
              (crear-binding id (subst sub-id value val)))]
           [else binding]))
       assigns))
