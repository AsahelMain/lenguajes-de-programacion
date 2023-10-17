#lang plai

(require "grammars.rkt")
;(require "parser.rkt")
(require "desugar.rkt")



(define-type CFSBAE-Val
  [num-v (n number?)]
  [bool-v (b boolean?)]
  [string-v (s string?)]
  [closure-v (args (listof symbol?)) (body CFSBAE?) (env Env?)])

(define-type Env
  [mt-env]
  [cons-env (id symbol?) (value CFSBAE-Val?) (rest-env Env?)])

;; interp :: CFSBAE x Env -> CFSBAE-Val
(define (interp expr env)
  (type-case CFSBAE expr
    [num (n) (num-v n)]
    [id (i) (lookup i env)]
    [bool (b) (bool-v b)]
    [strinG (s) (string-v s)]   
    [op (f args) (error '0)]
    [fun (params body) (closure-v params body env)]
    [app (f args) (let* ([fu (interp f env)]
                         [arg-val (map (lambda (x) (interp x env)) args)]
                         [ext-env (extend-env (closure-v-args fu) arg-val (closure-v-env fu))])
                    (interp (closure-v-body fu) ext-env))]
    [iF (test-expr then-expr else-expr) (let ([test-val (interp test-expr env)])
                                          (if (bool-v? test-val)
                                              (if (equal? test-val (bool-v #t))
                                                  (interp then-expr env)
                                                  (interp else-expr env))
                                              (error 'interp "La condición de una expresión if debe ser un booleano.")))]))
    

;; lookup :: symbol x Env -> CFSBAE-Val
(define (lookup sub-id env)
  (type-case Env env
    [mt-env () (error 'interp (format "Variable libre ~a" sub-id))]
    [cons-env (id value rest-env)
         (if (symbol=? sub-id id)
             value
             (lookup sub-id rest-env))]))

;; extend-env :: listof symbol x listof CFSBAE -> Env
(define (extend-env params args env)
  (if (not(= (length params) (length args)))
      (error'interp "Numero de argumentos y parametros distinto")
      (if (empty? params)
          env
          (cons-env (first params) (first args) (extend-env (cdr params) (cdr args) env)))))


(require racket/trace)
(trace interp)
(trace lookup)
(trace extend-env)