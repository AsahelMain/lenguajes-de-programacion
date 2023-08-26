#lang plai

#| Ejercicio 1 |#
#| Funcion que calcula el area de un cono circular recto, dada su generatriz y base del cono.
Se usa la formula dada en el pdf. |#
(define (area-total g d) (+ (* pi (/ d 2) g) (* pi (* (/ d 2) (/ d 2)))))

#| Ejercicio 2 |#
#| Función que indica si cuatro números están ordenados
   de forma estrictamente decremental |#
(define (decremental? a b c d)
  (and (> a b) (> b c) (> c d))
)

#| Ejercicio 3 |#
#| Funcion que dada una lista multiplica todos los elementos contenidos en la misma.
Es recursiva, multiplica la cabeza de cada cola resultante de la lista tras llamar la funcion,
el caso devuelve un uno para terminar de multiplicar.|#
(define (multiplica ls)
  (cond
    [(empty? ls) 1]
    [else (* (car ls) (multiplica (cdr ls)))]))

#| Ejercicio 4 |#
#| Funcion principal que calcula el área de un triángulo dados
sus tres lados. Se usa la fórmula de Herón para calcular dicha área |#
(define (area-heron a b c)
  (let ([sp (/ (+ a b c) 2)])
  (sqrt (* sp (- sp a)(- sp b)(- sp c))))
)

#| Ejercicio 5 |#
#| Predicado que indica si todos los números contenidos en una lista |#
#| son pares |#
(define (pares? lst)
  (cond [(empty? lst) #t]
        [else (and (even? (car lst)) (pares? (cdr lst)))]))

#| Ejercicio 6 |#
#| Función principal que filtra los elementos de una lista de acuerdo a un |#
#| predicado |#
(define (filtra-lista p ls)
  (filtra-lista-aux p ls empty))
#| Función auxiliar de filtra-lista. Filtra los elementos de una lista de acuerdo |#
#| a un predicado. La función se usa porque recibe como parámetro un acumulador para |#
#| así usar recursión de cola |#
(define (filtra-lista-aux p ls acc)
  (cond [(empty? ls) (reverse acc)]
        [(p (car ls)) (filtra-lista-aux p (cdr ls) (cons (car ls) acc))]
        [else (filtra-lista-aux p (cdr ls) acc)])
  )
