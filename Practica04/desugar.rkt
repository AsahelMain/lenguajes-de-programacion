#lang plai

(require "grammars.rkt")
;; (require "parser.rkt")

;; desugar :: CFWSBAE -> CFSBAE
(define (desugar expr)
  (type-case CFWSBAE expr
             [numS (n) (num n)]
             [idS (i) (id i)]
             [boolS (b) (bool b)]
             [strinGS (s) (strinG s)]
             [opS (f args) (op f (map desugar args))]
             [withS (bindings body)
                    (app (fun (map binding-id bindings) (desugar body))
                         (map (lambda (x) (desugar (binding-value x))) bindings))]
             [with*S (bindings body)
                     (desugar (with*S->withS expr))]
             [funS (params body) (fun params (desugar body))]
             [appS (f args) (app (desugar f) (map desugar args))]
             (iFS (test-expr then-expr else-expr)
                  (iF (desugar test-expr) (desugar then-expr) (desugar else-expr)))
             [conDS (conds else-expr)
                    (let* ([first-cond (first conds)]
                           [first-test (condition-test-expr first-cond)]
                           [first-then (condition-then-expr first-cond)])
                      (iF (desugar first-test) (desugar first-then)
                          (if (empty? (rest conds))
                              (desugar else-expr)
                              (desugar (conDS (rest conds) else-expr)))))]))

(define (with*S->withS expr)
  (let ([bindings (with*S-bindings expr)]
        [body (with*S-body expr)])
    (withS (list (first bindings))
           (if (empty? (rest bindings))
               body
               (with*S->withS (with*S (rest bindings) body))))))
