#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; TCFWSBAE x TypeContext -> Type
(define (typeof expr ctx)
  (type-case TCFWSBAE expr
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
             [else "TODO"]))

(define (conD->iF expr)
  (let ([conditions (conD-conditions expr)]
        [else-expr (conD-else-expr expr)])
    (if (empty? conditions)
        else-expr
        (iF (condition-test-expr (first conditions))
            (condition-then-expr (first conditions))
            (conD->iF (conD (rest conditions) else-expr))))))
