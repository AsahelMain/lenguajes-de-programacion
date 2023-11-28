#lang plai

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./verificador.rkt"))

;; Función auxiliar para pruebas, llama a parse y a typeof
;; con el contexto de tipos vacío.
;; test: SCFWBAE -> Type
(define (prueba exp)
  (typeof (parse exp) (phi)))

(display "PRUEBAS TYPEOF\n______________________________________________________________________________________________________________________________\n\n")

(test (prueba '#t) (booleanT))

(test (prueba '2) (numberT))

(test (prueba '{+ 1 2}) (numberT))

(test/exn (prueba '{+ 1 #f}) "typeof: Error en el parametro (bool #f). Tipo esperado: (numberT). Tipo dado: (booleanT)")

(test/exn (prueba '{and 1 #f}) "typeof: Error en el parametro (num 1). Tipo esperado: (booleanT). Tipo dado: (numberT)")

(test/exn (prueba '{> 0 0 0 0 #t}) "typeof: Error en el parametro (bool #t). Tipo esperado: (numberT). Tipo dado: (booleanT)")

(test/exn (prueba '{str-length 8}) "typeof: Error en el parametro (num 8). Tipo esperado: (stringT). Tipo dado: (numberT)")
                  
(test (prueba '{if #t 2 3}) (numberT))

(test (prueba '{with {{x : num 2} {y : bool #t} {z : num 1}} {if y x z}}) (numberT))

(test/exn (prueba '{if #t 2 #t}) "typeof: El tipo de la rama else tiene que ser el mismo que el de las ramas then")

(test/exn (prueba '{cond {#t 2} {#f 3} {else #t}}) "typeof: El tipo de la rama else tiene que ser el mismo que el de las ramas then")

(test/exn (prueba '{if 3 2 #t}) "typeof: El tipo de las condicionales tiene que ser boolean. Tipo dado: (numberT)")

(test (prueba '{fun {{x : num} {y : bool}} : num {if y x 0}}) (funT (list (numberT) (booleanT) (numberT))))

(test/exn (prueba '{fun {{x : num} {y : num}} : num {if y x 0}}) "typeof: El tipo de las condicionales tiene que ser boolean. Tipo dado: (numberT)")

(test (prueba '{{fun {{x : num} {y : bool}} : num {if y x 0}} 2 #t}) (numberT))

(test/exn (prueba '{{fun {{x : num} {y : bool}} : num {if y x 0}} 2 3})
          "typeof: El tipo de los parametros no es el esperado. Tipo dado: (numberT). Tipo esperado: (booleanT)")

(test/exn (prueba '{with {{foo : (num bool -> num) {fun {{x : num} {y : num}} : (num -> bool) {if {zero? x} y x}}}} 10})
          "typeof: El tipo de retorno de la función no coincide con el tipo de su cuerpo. Tipo dado: (numberT). Tipo esperado: (funT ((numberT) (booleanT))).")

(test/exn (prueba '{with {{foo : (str -> num) {fun {{x : str}} : num {str-length x}}}
                          {res : num {foo "Hola"}}}
                         res})
          "typeof: El identificador no se encuentra en el contexto.")

(test (prueba '{with* {{foo : (str -> num) {fun {{x : str}} : num {str-length x}}}
                       {res : num {foo "Hola"}}}
                      res})
      (numberT))

(test/exn (prueba '{with {{x : bool 10}}
                         x})
          "typeof: El tipo de la asignación no corresponde con el del valor. Tipo esperado: (booleanT). Tipo dado: (numberT).")
