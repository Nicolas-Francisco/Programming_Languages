#lang play
(require "t1-p2.rkt")
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


;---------------------------------- PARSER TESTS ----------------------------------; 
; num case
(test (parse 1)
      (num 1))
; id case
(test (parse 'x) 
      (id 'x))
; bool case
(test (parse #t) 
      (bool #t))
; binop case
(test (parse '{+ 1 2})  
      (binop + (num 1) (num 2)))
; unop case
(test (parse '{! #t})  
      (unop not (bool #t)))
; if case
(test (parse '{if c (+ 1 2) (+ 3 4)})   
      (if-expr (id 'c) (binop + (num 1) (num 2)) (binop + (num 3) (num 4))))
; with case
(test (parse '{with {{x 5} {y 7} {z 42}} z})   
      (with (list (list 'x Any (num 5))
                  (list 'y Any (num 7))
                  (list 'z Any (num 42)))
            (id 'z)))

(test (parse '{with {{x : Num 5} {y : Num 10}} {+ x y}})
      (with (list (list 'x Num (num 5))
                  (list 'y Num (num 10)))
            (binop + (id 'x) (id 'y))))
; define case
(test (parse '{define {sum3 x y z} {+ {+ x y} z}})
      (fundef 'sum3
              (list (arg-any 'x) (arg-any 'y) (arg-any 'z))
              Any
              (binop + (binop + (id 'x) (id 'y)) (id 'z))))
; app case
(test (parse '{sum3 1 2 3})
      (app 'sum3 (list (num 1) (num 2) (num 3))))

;--------------------------------- TYPEOF  TESTS ---------------------------------;
; num case
(test (typeof (num 1) '() (mtEnv))
      Num)
; id case
(test (typeof (id 'x) '() (aEnv 'x Num 5 mtEnv))
      Num)
(test/exn (typeof (id 'x) '() (mtEnv))
          "env-lookup-type: free identifier: x")
; bool case
(test (typeof (bool #t) '() (mtEnv))
      Bool)
; unop case
(test (typeof (parse'{! #t}) '()  (mtEnv))
      Bool)
(test/exn (typeof (parse '{! 5}) '()  (mtEnv))
          "Static type error: expected Bool found Num")

; binop case
(test (typeof (parse '{+ 2 3}) '() (mtEnv))
      Num)
(test (typeof (binop + (num 1) (num 2)) '() (mtEnv))
      Num)
(test/exn (typeof (parse '{+ #f 3})  '()  (mtEnv))
          "Static type error: expected Num found Bool")
(test/exn (typeof (parse '{&& #f 3}) '()  (mtEnv))
          "Static type error: expected Bool found Num")
(test/exn (typeof (parse '{> 10 #t}) '()  (mtEnv))
          "Static type error: expected Num found Bool")

; if case
(test (typeof (parse'{if #t #t #t}) '() (mtEnv))
      Bool)
(test/exn (typeof (parse '{if 1 #t #t}) '() (mtEnv))
          "Static type error: expected Bool found Num")
(test/exn (typeof (parse '{if #t 1 #t}) '() (mtEnv))
          "Static type error: different types")

; with case
(test (typeof (parse '{with {{x 5} {y : Bool #f} {z #f}} (&& y z)})
              '()
              (mtEnv))
      Bool)
(test/exn (typeof (parse '{with {{x 5} {y : Bool #f} {z #f}} (+ y z)})
                  '()
                  (mtEnv))
          "Static type error: id types does not match")
(test/exn (typeof (parse '{with {{x 5} {y : Bool #f} {z #f}} (&& x z)})
                  '()
                  (mtEnv))
          "Static type error: id types does not match")

; define case
(test/exn (typeof (parse '{one #t})
                  (list (parse '{define {one {x : Num}} 1}))
                  (mtEnv))
          "Static type error: expected Num found Bool")
(test (typeof (parse '{one 4})
              (list (parse '{define {one {x : Num}} 1}))
              (mtEnv))
      Any)
(test (typeof (parse '{one 4})
              (list (parse '{define {one {x : Num}} : Num 1}))
              (mtEnv))
      Num)
(test (typeof (parse '{f {> 3 4}})
              (list (parse '{define {f {p : Bool}} {&& p {! p}}}))
              (mtEnv))
      Any)