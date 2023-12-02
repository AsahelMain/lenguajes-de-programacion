#lang plai

(require "grammars.rkt")
(require "parser.rkt")
#|
Función encargada de evaluar los tipos de une expresion
|#
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
             [op (f args)
                 (let ([tipo-esperado (tipo-op f)]
                       [tipos-conseguidos (map (lambda (x) (typeof x ctx)) args)])
                   (if (andmap (lambda (x) (equal? x tipo-esperado)) tipos-conseguidos)
                       tipo-esperado
                       (let* ([tipo-distinto (obtener-tipo-distinto-param tipo-esperado tipos-conseguidos)]
                              [param-distinto (first (filter (lambda (x) (equal? tipo-distinto (typeof x ctx))) args))])
                         (error 'typeof "Error en el parametro ~v. Tipo esperado: ~v. Tipo dado: ~v" param-distinto tipo-esperado tipo-distinto))))]
             [fun (params return-type body)
                (let ([extended-ctx (foldl (lambda (x acc) (gamma (param-id x) (param-type x) acc))
                                           ctx
                                           params)])
                  (if (equal? (typeof body extended-ctx) return-type)
                      (funT (append (map param-type params) (list return-type)))
                      (error "errro")))]
             [app (f params)
                  (let* ([f-types (funT-params (typeof f ctx))]
                         [expected-types (take f-types (sub1 (length f-types)))]
                         [return-type (last f-types)]
                         [params-types (map (lambda (x) (typeof x ctx)) params)])
                    (if (not (equal? expected-types params-types))
                        (let* ([tipo-distinto (ormap (lambda (elem1 elem2) (not (= elem1 elem2)))
                                                     (map list expected-types params-types))]
                               [param-distinto (first (filter (lambda (x) (equal? tipo-distinto (typeof x ctx))) params))])
                          (error 'typeof "Error en el parametro ~v. Tipo esperado: ~v. Tipo dado: ~v" param-distinto return-type tipo-distinto))
                        return-type
                        ))]
             [with* (assigns body) (typeof (with*->with expr) ctx)]
             [else "TODO"]))
#|
Devuelve un parametro distinto de una lista al esperado
obtener-tipo-distinto-param: Param  list -> Param 
obtener-tipo-distinto-param 
|#
  (define (obtener-tipo-distinto-param esperado lst)
    (cond [(not (equal? (car lst) esperado)) (car lst)]
          [else (obtener-tipo-distinto-param esperado (cdr lst))]))

#|Convierte una expresion de conD a una iF

conD->iF: TCFWSBAE -> TCFWSBAE
|#
(define (conD->iF expr)
  (let ([conditions (conD-conditions expr)]
        [else-expr (conD-else-expr expr)])
    (if (empty? conditions)
        else-expr
        (iF (condition-test-expr (first conditions))
            (condition-then-expr (first conditions))
            (conD->iF (conD (rest conditions) else-expr))))))

(define (with->app w ctx)
  (let ([bindings (with-bindings w)]
        [body (with-body w)])
    (app (fun (map (lambda (x) (param (binding-id x) (binding-type x))) bindings)
            (typeof body ctx)))))
#|Función auxiliar de type transforma una expresión TCFWSBAE with* a expresiones
with anidadas. Se usa para simplificar los procedimientos que involucran a with*.

with*->with: TCFWSBAE -> TCFWSBAE
|#
(define (with*->with expr)
  (let ([bindings (with*-bindings expr)]
        [body (with*-body expr)])
    (with (list (first bindings))
          (if (empty? (rest bindings))
              body
              (with*->with (with* (rest bindings) body))))))

#| Función auxiliar de type.
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

#|
Devuelve el tipo de una operacion
tipo-op : op -> Type
|#
(define (tipo-op op)
  (let ([ops-con-num '(+ - / * min max expt sqrt sub1 add1 < > <= >= = zero? modulo)]
        [ops-con-bool '(not and or)]
        [ops-con-str '(str-length string?)])
    (cond
      [(ormap (lambda (x) (eq? (translate x) op)) ops-con-num)
       (numberT)]
      [(ormap (lambda (x) (eq? (translate x) op)) ops-con-bool)
       (booleanT)]
      [(ormap (lambda (x) (eq? (translate x) op)) ops-con-str)
       (stringT)])))
