#|@author{Arturo González Peñaloza 319091193}
@author{Asahel Said Main Cerezo 319260658}
@author{Emilio Arsenio Raudry Rico 318289276}
|#
#lang plai

(define-type Binding
  [binding (id symbol?) (value RCFSBAE?)])

(define-type RCFSBAE
  [num (n number?)]
  [id (i symbol?)]
  [bool (b boolean?)]
  [strinG (s string?)]
  [op (f procedure?) (args (listof RCFSBAE?))]
  [fun (params (listof symbol?)) (body RCFSBAE?)]
  [app (f RCFSBAE?) (args (listof RCFSBAE?))]
  [iF (test-expr RCFSBAE?) (then-expr RCFSBAE?) (else-expr RCFSBAE?)]
  [rec (bindings (listof Binding?)) (body RCFSBAE?)])