#lang plai

#| Ejercicio 2 |#
#| Función que indica si cuatro números están ordenados
   de forma estrictamente decremental |#
(define (decremental? a b c d)
  (and (> a b) (> b c) (> c d))
)


#| Ejercicio 4 |#
#| Funcion principal que calcula el área de un triángulo dados
sus tres lados. Se usa la fórmula de Herón para calcular dicha área |#
(define (area-heron a b c)
  (let ([sp (/ (+ a b c) 2)])
  (sqrt (* sp (- sp a)(- sp b)(- sp c))))
)