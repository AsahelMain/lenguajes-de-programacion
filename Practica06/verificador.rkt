#lang plai

(require "grammars.rkt")
(require "parser.rkt")

(lookup 'y (gamma 'y (booleanT )(gamma 'x (numberT) (phi))))
;; TCFWSBAE x TypeContext -> Type
(define (typeof expr ctx)
  (type-case TCFWSBAE expr
             [id (i) (lookup i ctx)]
             [num (n) (numberT)]
             [bool (b) (booleanT)]
             [strinG (s) (stringT)]
             [iF (test-expr then-expr else-expr)
                 (let ([tipo-test (typeof test-expr ctx)])
                   (if (not (booleanT? tipo-test))
                       (error 'typeof "El tipo de las condicionales tiene que ser boolean. Tipo dado: ~v" tipo-test)
                       (let ([tipo-then (typeof then-expr ctx)]
                             [tipo-else (typeof else-expr ctx)])
                         (if (not (equal? tipo-then tipo-else))
                             (error 'typeof "El tipo de la rama else tiene que ser el mismo que el de las ramas then")
                             tipo-else))))]
             [conD (conditions else-expr)
                   (typeof (conD->iF expr) ctx)]
             [fun (params return-type body)
                (funT (append (map param-type params) (list return-type)))]
             [app (f params)
                  (let* ([f-types (funT-params (typeof f ctx))]
                         [expected-types (take f-types (sub1 (length f-types)))]
                         [return-type (last f-types)]
                         [params-types (map (lambda (x) (typeof x ctx)) params)])
                    (if (not (equal? expected-types params-types))
                        (error 'typeof "El tipo de los parámetros no es el esperado." )
                        return-type)
                    )]
             [else "TODO"]))


(define (conD->iF expr)
  (let ([conditions (conD-conditions expr)]
        [else-expr (conD-else-expr expr)])
    (if (empty? conditions)
        else-expr
        (iF (condition-test-expr (first conditions))
            (condition-then-expr (first conditions))
            (conD->iF (conD (rest conditions) else-expr))))))

#|Función auxiliar de interp
Busca un símbolo en el contexto y devuelve su tipo.
Si no lo encuentra regresa un error.
lookup :: symbol x TypeContext -> Type
|#
(define (lookup sub-id env)
  (type-case TypeContext env
             [phi () (error 'interp (format "Variable libre ~a" sub-id))]
             [gamma (id type rest-context)
                    (if (symbol=? sub-id id)
                        type
                        (lookup sub-id rest-context))]))
