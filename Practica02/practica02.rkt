#|@author{Arturo González Peñaloza 319091193}
@author{Asahel Said Main Cerezo 319260658}
@author{Emilio Arsenio Raudry Rico 318289276}|#
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
#| Función que dados dos puntos calcula
   la distancia entre ambos
   distancia :: Punto Punto -> number|#
(define (distancia p q)
  (match (list p q)
    [(list (punto x1 y1) (punto x2 y2)) (sqrt (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1))))]
    [_ (error 'punto-medio "Ambos argumentos deben ser del tipo Punto")]))

#| Ejercicio 2 |#
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
#| Funcion que recibe una lista de elementos que pueden ser otras listas
   y devuelve una lista que contenga a todos los elementos de la lista
   aplana :: Lista -> Lista|#
(define (aplana ls)
  (type-case Lista ls
    (Vacia () (Vacia))
    (Cons (cabeza resto) (cond
                           [(Lista? cabeza) (concatena (aplana cabeza) (aplana resto))]
                           [else (Cons cabeza (aplana resto))]))))

#| Funcion que concatena dos listas en una sola
   concatena :: Lista  Lista -> Lista |#

(define (concatena ls ks)
  (match (list ls ks)
    [(list (Vacia) _) ks]
    [(list (Cons cabeza1 resto1) _) (Cons cabeza1 (concatena resto1 ks))]))  

#| Ejercicio 3 |#
(define-type ArbolBinarioDeBusqueda
  [ArbolVacio]
  [ABB (elemento number?)
       (izq ArbolBinarioDeBusqueda?)
       (der ArbolBinarioDeBusqueda?)])

;; Ejercicio 3.a)

#|Recibe un árbol y un elemento, devuelve el mismo árbol pero sin ese elemento.
  Comienza ubicando al nodo con el elemento a eliminar.
  elimina :: ArbolBinarioDeBusqueda elemento -> ArbolBinarioDeBusqueda|#
(define (elimina ar e)
  (type-case ArbolBinarioDeBusqueda ar
             (ArbolVacio () ar)
             (ABB (elemento izq der)
                  (cond [(< e elemento) (ABB elemento izq (elimina der e))]
                        [(> e elemento) (ABB elemento (elimina izq e) der)]
                        [else (elimina-aux ar)]))))

#|Función auxiliar de elimina. Elimina el elemento del árbol dependiendo
  del estado de sus árboles izquierdo y derecho.
  elimina :: ArbolBinarioDeBusqueda elemento -> ArbolBinarioDeBusqueda|#
(define (elimina-aux ar)
  (type-case ArbolBinarioDeBusqueda ar
             (ArbolVacio () ar)
             (ABB (elemento izq der)
                  (cond
                    [(ArbolVacio? izq) der]
                    [(ArbolVacio? der) izq]
                    [else (let ((sucesor (obtenMinimo izq)))
                            (ABB (ABB-elemento sucesor) (elimina izq (ABB-elemento sucesor)) der))]))))

#| Devuelve el árbol cuyo elemento es el elemento mínimo
   obtenMinimo :: ArbolBinarioDeBusqueda -> ArbolBinarioDeBusqueda|#
(define (obtenMinimo ar)
  (type-case ArbolBinarioDeBusqueda ar
             (ArbolVacio () ar)
             (ABB (elemento _ der)
                  (cond
                    [(ArbolVacio? der) ar]
                    [else (obtenMinimo der)]))))

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
      ;; La función 'group-by preserva el orden de la lista original.
      (let* ((elementos-agrupados (group-by identity ls))
             ;; Lista de listas donde el primer elemento de las sublistas
             ;; es elemento de la lista original y el segundo es su número de repeticiones.
             (repeticiones-elementos
              (map (lambda (x) (list (first x) (length x))) elementos-agrupados)))
        ;; Ordena establemente la lista con la comparación >, usando
        ;; el segundo elemento de la lista como llave (el número de
        ;; repeticiones) y devuelve el primer elemento de la primera lista.
        (first (first (sort repeticiones-elementos #:key second >))))))
