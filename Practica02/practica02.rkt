#lang plai

(define (any? x)
  #t)

#| Ejercicio 1 |#
(define-type Punto
  [punto (x number?) (y number?)])

;; Ejercicio 1.a)
#| Función que dados dos puntos
   calcula el punto medio entre estos.
   Simplemente se usa la fórmula para calcular
   el punto medio entre dos puntos
   
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
#| Calcula la cantidad de elementos de una lista.
   longitud :: Lista -> number |#
(define (longitud ls)
  (longitud-aux 0 ls))
#| Función auxiliar de longitud para usar recursión de cola.
   longitud-aux :: number Lista -> number |#
(define (longitud-aux accum ls)
  (type-case Lista ls
             ;; Si es vacía se devuelve el acumulador.
             (Vacia () accum)
             ;; Se invoca la función sobre la cola de la lista
             ;; e incrementando en uno el contador.
             (Cons (_ resto) (longitud-aux (add1 accum) resto))))

;; Ejercicio 2.b)
#| Indica si un elemento está en una lista.
   pertenece? :: any Lista -> boolean |#
(define (pertenece? e ls)
  (type-case Lista ls
             (Vacia () #f)
             ;; El elemento pertenece solo si es igual a la cabeza de la lista
             ;; o si pertenece a la cola de la lista.
             (Cons (cabeza resto) (or (equal? cabeza e) (pertenece? e resto)))))


;; Ejercicio 2.c)
#| Función que dadas dos listas intercala
   sus elementos. La función principal
   verifica que las dos expresiones pasadas
   como argumentos sean listas.
   
   intercala :: Lista Lista -> Lista    |#
(define (intercala ls ks)
  (cond
    [(and (Lista? ls) (Lista? ks)) (intercala-aux ls ks)]
    [else (error 'intercala "ambos elementos deben ser del tipo Lista")]))
#| Función auxiliar de intercala
   Utiliza recursión para agregar alternadamente elementos
   de las dos listas. Para lograr esto se intercambia el
   orden en el que se pasan las listas a la llamada recursiva.
   
   intercala-aux :: Lista Lista -> Lista
|#
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
#| Aplica una función a todos los elementos de un árbol binario de búsqueda
   y regresa el árbol resultado.
   mapea-arbol :: ArbolBinarioDeBusqueda procedure ->ArbolBinarioDeBusqueda |#
(define (mapea-arbol ab f)
  (type-case ArbolBinarioDeBusqueda ab
             (ArbolVacio () ab)
             (ABB (elemento izq der)
                  ;; Se aplica la función solo sobre el elemento del árbol.
                  (ABB (f elemento) (mapea-arbol izq f) (mapea-arbol der f)))))

;; Ejercicio 3.c)
#| Funcion que dado un árbol binario regresa
   una lista con todas sus hojas.
   Utiliza recursión, si el elemento es una hoja entonces
   se crea una lista con ese elemento, si no es una hoja
   entonces se hace una llamada recursiva a los subárboles
   y las listas que resulten de esas llamadas se concatenan
   
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
#| Regresa el primer elemento con mayor número de apariciones en una lista.
   mas-repetido :: (listof any) ->any |#
(define (mas-repetido ls)
  (if (empty? ls)
      (error 'mas-repetido "La lista está vacía.")
      ;; Agrupa los elementos de la lista por sus identidades.
      (let* ((elementos-agrupados (group-by identity ls))
             ;; Lista con elemento y repeticiones.
             (repeticiones-elementos
              (map (lambda (x) (list (first x) (length x))) elementos-agrupados)))
        ;; Ordena establemente la lista con la comparación > y devuelve el primer
        ;; elemento de la primera lista.
        (first (first (sort repeticiones-elementos #:key second >))))))
