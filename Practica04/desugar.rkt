#lang plai

(require "grammars.rkt")
(require "parser.rkt")
#|Ejercicio 4
  Función que recibe una expresión CFWSBAE y la transforma en una expresión
  CFSBAE
  desugar :: CFWSBAE -> CFSBAE
|#
(define (desugar expr)
  (type-case CFWSBAE expr
             [numS (n) (num n)]
             [idS (i) (id i)]
             [boolS (b) (bool b)]
             [strinGS (s) (strinG s)]
             [opS (f args) (op f (map desugar args))]
             [withS (bindings body)
                    ;; La expresión se transforma en una aplicación de función.
                    (app (fun (map binding-id bindings) (desugar body))
                         (map (lambda (x) (desugar (binding-value x)))
                              bindings))]
             [with*S (bindings body)
                     ;; La expresión se transforma en withs anidados y
                     ;; se aplica desugar sobre la nueva expresión.
                     (desugar (with*S->withS expr))]
             [funS (params body) (fun params (desugar body))]
             [appS (f args) (app (desugar f) (map desugar args))]
             (iFS (test-expr then-expr else-expr)
                  (iF (desugar test-expr)
                      (desugar then-expr)
                      (desugar else-expr)))
             ;; La expresión se transforma en ifs anidados.
             [conDS (conds else-expr)
                    (if (empty? conds)
                        (desugar else-expr)
                        (iF (desugar (condition-test-expr (first conds)))
                            (desugar (condition-then-expr (first conds)))
                            (desugar (conDS (rest conds) else-expr))))]))
#|Función auxiliar transforma una expresión CFWSBAE with* a expresiones
  with anidadas. Se usa para simplificar los procedimientos que involucran a with*.
  with*->with: CFWSBAE -> CFWSBAE
|#
(define (with*S->withS expr)
  (let ([bindings (with*S-bindings expr)]
        [body (with*S-body expr)])
    (withS (list (first bindings))
           (if (empty? (rest bindings))
               body
               (with*S->withS (with*S (rest bindings) body))))))
