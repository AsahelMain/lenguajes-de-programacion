#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "desugar.rkt")

(define-type CFSBAE-Val
  [num-v (n number?)]
  [bool-v (b boolean?)]
  [string-v (s string?)]
  [closure-v (args (listof symbol?)) (body CFSBAE?) (env Env?)])

(define-type Env
  [mt-env]
  [cons-env (id symbol?) (value CFSBAE-Val?) (rest-env Env?)])


#|Ejercicio 4
  Función que recibe una expresión CFSBAE y la interpreta
  interp :: CFSBAE x Env -> CFSBAE-Val
|#
(define (interp expr env)
  (type-case CFSBAE expr
    [num (n) (num-v n)]
    [id (i) (lookup i env)]
    [bool (b) (bool-v b)]
    [strinG (s) (string-v s)]   
    [op (f args) (let* ([args-interpretados (map (lambda(x)(interp x env)) args)]
                        ;; Se convierten los argumentos de CFSBAE-Val a valores de Racket
                        [args-convertidos (map (lambda (x) (CFSBAE-Val->RacketValue x)) args-interpretados)])                   
                   ;; Se revisa que los tipos de los argumentos dados sean validos.
                   ;; En caso de no serlo se imprime el procedimiento con el tipo de argumento
                   ;; esperado y el recibido.
                   ;; Se decidió manejar los errores de esta forma debido a que
                   ;; Racket lo hace de una forma similar.
                   (when (verifica-argumentos f args-convertidos)
                     (let ([resultado (apply f args-convertidos)])
                         ;; Se convierten los valores de Racket a valores de tipo CFSBAE-val
                         (RacketValue->CFSBAE-Val resultado))))
                       ]
    [fun (params body) (closure-v params body env)]
    [app (f args) (let* ([fu (interp f env)]
                         ;; Se interpretan los argumentos de la función con el entorno actual
                         [arg-val (map (lambda (x) (interp x env)) args)]
                         ;; Se crea un nuevo entorno con los argumentos del closure de función,
                         ;; los argumentos interpretados y el entorno del closure de función
                         [ext-env (extend-env (closure-v-args fu) arg-val (closure-v-env fu))])
                    ;; Se interpreta el cuerpo del closure de la función con el entorno extendido
                    ;; De esta forma se logra el alcance estático en el lenguaje
                    (interp (closure-v-body fu) ext-env))]
    [iF (test-expr then-expr else-expr) (let ([test-val (interp test-expr env)])
                                          (if (bool-v? test-val)
                                              (if (equal? test-val (bool-v #t))
                                                  (interp then-expr env)
                                                  (interp else-expr env))
                                              (error 'interp "La condición de una expresión if debe ser un booleano.")))]))
    
#|Función auxiliar de interp
  Busca un símbolo en el entorno y devuelve su valor asociado.
  Si no lo encuentra regresa un error.
  lookup :: symbol x Env -> CFSBAE-Val
|#
(define (lookup sub-id env)
  (type-case Env env
    [mt-env () (error 'interp (format "Variable libre ~a" sub-id))]
    [cons-env (id value rest-env)
         (if (symbol=? sub-id id)
             value
             (lookup sub-id rest-env))]))

#| Función auxiliar de interp
   Recibe una lista de símbolos, una lista de valores CFSBAE
   y un entorno. Construye un entorno con parejas de símbolos
   y valores CFSBAE sobre el entorno que se pasa como parámetro
   ;; extend-env :: listof symbol x listof CFSBAE x Env -> Env
|#

(define (extend-env params args env)
  (if (not(= (length params) (length args)))
      (error'interp "Numero de argumentos y parametros distinto")
      (if (empty? params)
          env
          (cons-env (first params) (first args) (extend-env (cdr params) (cdr args) env)))))


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
      [(ormap (lambda (x) (eq? (translate x) f)) ops-con-num)
       (if (andmap number? args)
           #t
           (error 'interp "~a: error: violación de contrato\nesperado: num\ndado: ~a"
                  f (findf (lambda (x) (not (number? x))) args)))]
      [(ormap (lambda (x) (eq? (translate x) f)) ops-con-int) (if (andmap integer? args)
                                                             #t
                                                             (error 'interp "~a: error: violación de contrato\nesperado: entero\ndado: ~a"
                                                                   f (findf (lambda (x) (not (integer? x))) args)))]
      [(ormap (lambda (x) (eq? (translate x) f)) ops-con-bool) (if (andmap boolean? args)
                                                              #t
                                                              (error 'interp "~a: error: violación de contrato\nesperado: bool\ndado: ~a"
                                                                   f (findf (lambda (x) (not (boolean? x))) args)))]
      [(ormap (lambda (x) (eq? (translate x) f)) ops-con-str) (if (andmap string? args)
                                                             #t
                                                             (error 'interp "~a: error: violación de contrato\nesperado: str\ndado: ~a"
                                                                   f (findf (lambda (x) (not (string? x))) args)))]
      [else #t])))


#|Función que convierte valores CFSBAE-Val a valores de Racket
  CFSBAE-Val->RacketValue :: CFSBAE-Val -> number | boolean | string
|#
(define (CFSBAE-Val->RacketValue val)
  (match val
    [(num-v n) n]
    [(bool-v b) b]
    [(string-v s) s]
    [_ "~val: error: no es un valor valido"]))

#|Función que convierte valores de Racket a valores de tipo CFSBAE-Val
  RacketValue->CFSBAE-Val :: number | boolean | string -> CFSBAE-Val
|#

(define (RacketValue->CFSBAE-Val val)
  (cond
    [(number? val) (num-v val)]
    [(boolean? val) (bool-v val)]
    [(string? val) (string-v val)]
    [else "~val: error: no es un valor valido"]))
    
