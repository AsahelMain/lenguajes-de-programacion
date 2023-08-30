#lang plai

(define (any? x)
  #t)

(define-type Punto
  [punto (x number?) (y number?)])

;; Ejercicio 1.a)
(define (punto-medio p q)
  (error 'punto-medio "Sin implementar"))

;; Ejercicio 1.b)
(define (distancia p q)
  (error 'distancia "Sin implementar"))

(define-type Lista
    [Vacia]
    [Cons (cabeza any?) (resto Lista?)])

;; Ejercicio 2.a)
(define (longitud ls)
  (longitud-aux 0 ls))
(define (longitud-aux accum ls)
  (type-case Lista ls
             (Vacia () accum)
             (Cons (_ resto) (longitud-aux (+ 1 accum) resto))))

;; Ejercicio 2.b)
(define (pertenece? e ls)
  (type-case Lista ls
             (Vacia () #f)
             (Cons (cabeza resto) (or (= cabeza e) (pertenece? e resto)))))


;; Ejercicio 2.c)
(define (intercala ls ks)
  (error 'intercala "Sin implementar"))

;; Ejercicio 2.d)
(define (aplana ls)
  (error 'aplana "Sin implementar"))

(define-type ArbolBinarioDeBusqueda
  [ArbolVacio]
  [ABB (elemento number?)
       (izq ArbolBinarioDeBusqueda?)
       (der ArbolBinarioDeBusqueda?)])

;; Ejercicio 3.a)
(define (elimina e a)
  (error 'elimina "Sin implementar"))

;; Ejercicio 3.b)
(define (mapea-arbol ab f)
  (type-case ArbolBinarioDeBusqueda ab
             (ArbolVacio () ab)
             (ABB (elemento izq der)
                  (ABB (f elemento) (mapea-arbol izq f) (mapea-arbol der f)))))

;; Ejercicio 3.c)
(define (hojas ar)
  (error 'hojas "Sin implementar"))

;; Punto Extra
(define (mas-repetido ls)
  (if (empty? ls)
      (error 'ls "La lista está vacía.")
      (let* ((elementos-agrupados (group-by identity ls))
             (repeticiones-elementos
              (map (lambda (x) (list (first x) (length x))) elementos-agrupados)))
        (first (first (sort repeticiones-elementos #:key second >))))))
