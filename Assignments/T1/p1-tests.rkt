#lang play
(require "t1.rkt")
(print-only-errors #t)

;----------------------------------------------------------------------------------;
#|
                        CC4101 - LENGUAJES DE PROGRAMACIÓN
                                     TAREA 1

                             Profesor: Éric Tanter
                     Auxiliares: Bryan Ortiz - Tomas Vallejos
                         Estudiante: Nicolás García Ríos
|#
;----------------------------------------------------------------------------------;


;----------------------------------- RUN  TESTS -----------------------------------;
; Programa de Ejemplo 1 (enunciado)
(test (run '{{define {sum x y z} {+ x {+ y z}}}
             {define {max x y} {if {< x y} y x}}
             {with {{x 9}} {sum {max x 6} 2 -10} }})
      1)   
; Programa de Ejemplo 2 (enunciado)
(test (run '{{with {{x 5} {y 7} {z 42}} z}})
      42)
; Programa de Ejemplo 3 (enunciado)
(test (run '{{define {triple x} {* 3 x}}
             {define {add2 x} {+ 2 x}}
             {add2 {triple 2}}})
      8)
; Programa de Ejemplo 4 - undefined function call
(test/exn (run '{{sum 1 2 3}})
          "function not found: sum")
; Programa de Ejemplo 5 - free identifier
(test/exn (run '{{define {triple x} {* 3 x}}
                 {triple x}})
          "env-lookup: free identifier: x")
; Programa de Ejemplo 6 - app aplication using with
(test (run '{{define {triple x} {* 3 x}}
             {with {{x 3}} {triple x}}})
      9)
; Programa de Ejemplo 7 - if aplication on true branch
(test (run '{{define {triple x} {* 3 x}}
             {if {< 1 2} {triple 3} {triple 4}}})
      9)
; Programa de Ejemplo 8 - if aplication on false branch
(test (run '{{define {triple x} {* 3 x}}
             {if {< 2 1} {triple 3} {triple 4}}})
      12)
; Programa de Ejemplo 9 - if aplication with an operation as condition
(test (run '{{if {&& #t #f} #t #f}})
      #f)
; Programa de Ejemplo 10 - Unary expresion with boolean type error
(test/exn (run '{{add1 #t}})
          "Runtime type error: expected Number found Boolean")
; Programa de Ejemplo 11 - Unary expresion with number type error
(test/exn (run '{{! 1}})
          "Runtime type error: expected Boolean found Number")
; Programa de Ejemplo 12 - Binary expresion with boolean type error
(test/exn (run '{{+ 1 #t}})
          "Runtime type error: expected Number found Boolean")
; Programa de Ejemplo 13 - Binary expresion with number type error
(test/exn (run '{{&& 1 #t}})
          "Runtime type error: expected Boolean found Number")