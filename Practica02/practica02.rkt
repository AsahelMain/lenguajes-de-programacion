#lang plai

(define (any? x)
  #t)

#| Ejercicio 1 |#
(define-type Punto
  [punto (x number?) (y number?)])

;; Ejercicio 1.a)
#| Función que dados dos puntos
   calcula el punto medio entre estos
   punto-medio :: Punto Punto -> Punto  |#
(define (punto-medio p q)
  (match (list p q)
    [(list (punto x1 y1) (punto x2 y2))
           (punto (exact->inexact(/ (+ x1 x2) 2)) (exact->inexact(/ (+ y1 y2) 2)))]
    [_ (error 'punto-medio "Ambos argumentos deben ser del tipo Punto")]))

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
#| Función que dadas dos listas intercala
   sus elementos
   intercala :: Lista Lista -> Lista    |#
(define (intercala ls ks)
  (cond
    [(and (Lista? ls) (Lista? ks)) (intercala-aux ls ks)]
    [else (error 'intercala "ambos elementos deben ser del tipo Lista")]))
(define (intercala-aux ls ks)
  (match (list ls ks)
    [(list (Vacia) (Vacia)) (Vacia)]
    [(list (Vacia) (Cons cabeza2 resto2)) ks]
    [(list (Cons cabeza1 resto1) (Vacia)) ls]
    [(list (Cons cabeza1 resto1) _) (Cons cabeza1 (intercala-aux ks resto1))]))
  

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
#| Funcion que dado un árbol binario regresa
   una lista con todas sus hojas
   hojas :: ArbolBinarioDeBusqueda -> (listof any) |#
(define (hojas ar)
  (type-case ArbolBinarioDeBusqueda ar
    [ArbolVacio () '()]
    [ABB (elemento izq der)
           (cond
             [(and (equal? izq (ArbolVacio)) (equal? der (ArbolVacio)))
               (list elemento)]
             [else
               (append (hojas izq) (hojas der))])]))
       

;; Punto Extra
(define (mas-repetido ls)
  (if (empty? ls)
      (error 'ls "La lista está vacía.")
      (let* ((elementos-agrupados (group-by identity ls))
             (repeticiones-elementos
              (map (lambda (x) (list (first x) (length x))) elementos-agrupados)))
        (first (first (sort repeticiones-elementos #:key second >))))))
