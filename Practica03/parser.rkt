#lang plai

(require "grammars.rkt")

(define (parse s-exp)
  (cond
    [(number? s-exp) (num s-exp)]
    [(symbol? s-exp) (id s-exp)]
    [(boolean? s-exp) (bool s-exp)]
    [(string? s-exṕ) (str s-exp)]
    [(list? s-exp)
     (let ([head (car s-exp)])
       (case (head)
         [(+) (if (> (length (cdr s-exp)) 0) (op #<procedure:+> (map parse(cdr s-exp))) (error 'interp "parse: La operacion + espera mas de 0 argumentos."))]
         [(-) (if (> (length (cdr s-exp)) 0) (op #<procedure:-> (map parse(cdr s-exp))) (error 'interp "parse: La operacion - espera mas de 0 argumentos."))]
         [(/) (if (> (length (cdr s-exp)) 0) (op #<procedure:/> (map parse(cdr s-exp))) (error 'interp "parse: La operacion / espera mas de 0 argumentos."))]
         [(*) (if (> (length (cdr s-exp)) 0) (op #<procedure:*> (map parse(cdr s-exp))) (error 'interp "parse: La operacion * espera mas de 0 argumentos."))]
         [(modulo) (if (= (length (cdr s-exp)) 2) (op #<procedure:modulo> (map parse(cdr s-exp))) (error 'interp (string-append ("parse: La operacion modulo espera 2 argumentos, Numero de argumentos dados: ") (number->string (length(cdr s-exp))))))]
         [(min) (if (> (length (cdr s-exp)) 0) (op #<procedure:min> (map parse(cdr s-exp))) (error 'interp "parse: La operacion min espera mas de 0 argumentos."))]
         [(max) (if (> (length (cdr s-exp)) 0) (op #<procedure:max> (map parse(cdr s-exp))) (error 'interp "parse: La operacion max espera mas de 0 argumentos."))]
         [(expt) (if (= (length (cdr s-exp)) 2) (op #<procedure:expt> (map parse(cdr s-exp))) (error 'interp (string-append ("parse: La operacion expt espera 2 argumentos, Numero de argumentos dados: ") (number->string (length(cdr s-exp))))))]
         [(sqrt) (if (> (length (cdr s-exp)) 0) (op #<procedure:sqrt> (map parse(cdr s-exp))) (error 'interp "parse: La operacion sqrt espera mas de 0 argumentos."))]
         [(sub1) (if (= (length (cdr s-exp)) 1) (op #<procedure:sub1> parse(cdr s-exp)) (error 'interp (string-append ("parse: La operacion sub1 espera 1 argumento, Numero de argumentos dados: ") (number->string (length(cdr s-exp))))))]
         [(add1) (if (= (length (cdr s-exp)) 1) (op #<procedure:add1> parse(cdr s-exp)) (error 'interp (string-append ("parse: La operacion add1 espera 1 argumento, Numero de argumentos dados: ") (number->string (length(cdr s-exp))))))]
         [(<) (if (> (length (cdr s-exp)) 0) (op #<procedure:<> (map parse(cdr s-exp))) (error 'interp "parse: La operacion < espera mas de 0 argumentos."))]
         [(>) (if (> (length (cdr s-exp)) 0) (op #<procedure:>> (map parse(cdr s-exp))) (error 'interp "parse: La operacion > espera mas de 0 argumentos."))]
         [(<=) (if (> (length (cdr s-exp)) 0) (op #<procedure:<=> (map parse(cdr s-exp))) (error 'interp "parse: La operacion <= espera mas de 0 argumentos."))]
         [(>=) (if (> (length (cdr s-exp)) 0) (op #<procedure:+> (map parse(cdr s-exp))) (error 'interp "parse: La operacion >= espera mas de 0 argumentos."))]
         [(=) (if (> (length (cdr s-exp)) 0) (op #<procedure:=> (map parse(cdr s-exp))) (error 'interp "parse: La operacion = espera mas de 0 argumentos."))]
         [(not) (if (= (length (cdr s-exp)) 1) (op #<procedure:not> parse(cdr s-exp)) (error 'interp (string-append ("parse: La operacion not espera 1 argumento, Numero de argumentos dados: ") (number->string (length(cdr s-exp))))))]
         [(and) (if (> (length (cdr s-exp)) 0) (op #<procedure:and> (map parse(cdr))) (error 'interp "parse: La operacion and espera mas de 0 argumentos."))]
         [(or) (if (> (length (cdr s-exp)) 0) (op #<procedure:or> (map parse(cdr))) (error 'interp "parse: La operacion or espera mas de 0 argumentos."))]
         [(zero?) (if (= (length (cdr s-exp)) 1) (op #<procedure:zero?> parse(cdr s-exp)) (error 'interp (string-append ("parse: La operacion zero? espera 1 argumento, Numero de argumentos dados: ") (number->string (length(cdr s-exp))))))]
         [(num?) (if (= (length (cdr s-exp)) 1) (op #<procedure:num?> parse(cdr s-exp)) (error 'interp (string-append ("parse: La operacion num? espera 1 argumento, Numero de argumentos dados: ") (number->string (length(cdr s-exp))))))]
         [(str?) (if (= (length (cdr s-exp)) 1) (op #<procedure:str> parse(cdr s-exp)) (error 'interp (string-append ("parse: La operacion str? espera 1 argumento, Numero de argumentos dados: ") (number->string (length(cdr s-exp))))))]
         [(bool?) (if (= (length (cdr s-exp)) 1) (op #<procedure:bool> parse(cdr s-exp)) (error 'interp (string-append ("parse: La operacion bool? espera 1 argumento, Numero de argumentos dados: ") (number->string (length(cdr s-exp))))))]
         [(str-length) (if (= (length (cdr s-exp)) 1) (op #<procedure:str-length> parse(cdr s-exp)) (error 'interp (string-append ("parse: La operacion str-length espera 1 argumento, Numero de argumentos dados: ") (number->string (length(cdr s-exp))))))]
         [(with) ]
         [(with*)]))
     ]
    )
  )
