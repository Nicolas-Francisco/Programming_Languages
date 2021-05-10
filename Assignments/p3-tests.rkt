#lang play
(require "t1-p3.rkt")
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
              mtEnv)
      Bool)
(test (typeof (parse '{with {{x 5} {y : Bool #f} {z #f}} (&& x z)})
                  '()
                  mtEnv)
      Bool)
(test/exn (typeof (parse '{with {{x 5} {y : Bool #f} {z #f}} (+ y z)})
                  '()
                  mtEnv)
          "Static type error: expected Num found Bool")
(test/exn (typeof (parse '{with {{x : Num 5} {y : Bool #f} {z #f}} (&& x z)})
                  '()
                  mtEnv)
          "Static type error: expected Bool found Num")

; define case
(test/exn (typeof (parse '{one #t})
                  (list (parse '{define {one {x : Num}} 1}))
                  (mtEnv))
          "Static type error: expected Num found Bool")
(test (typeof (parse '{one 4})
              (list (parse '{define {one {x : Num}} 1}))
              mtEnv)
      Any)
(test (typeof (parse '{one 4})
              (list (parse '{define {one {x : Num}} : Num 1}))
              (mtEnv))
      Num)
(test (typeof (parse '{f {> 3 4}})
              (list (parse '{define {f {p : Bool}} {&& p {! p}}}))
              (mtEnv))
      Any)

;---------------------------------  RUN  TESTS  ---------------------------------;

; tipo declarado Num en vez de Bool
(test/exn (run-typecheck '{{define {gt10 x} : Num {+ x 10}}
                           {define {positive {x @ gt10}} : Num {- x 10}}
                           {positive 15}})
          "Static contract error: invalid type for gt10")

; tipo declarado Any en vez de Bool
(test/exn (run-typecheck '{{define {gt10 x} : Any {+ x 10}}
                           {define {positive {x @ gt10}} : Num {- x 10}}
                           {positive 15}})
          "Static contract error: invalid type for gt10")

; tipo no declarado
(test/exn (run-typecheck '{{define {gt10 x} {+ x 10}}
                           {define {positive {x @ gt10}} : Num {- x 10}}
                           {positive 15}})
          "Static contract error: invalid type for gt10")

; recibe mas de un argumento
(test/exn (run-typecheck '{{define {gt10 x y} : Bool {> x 10}}
                           {define {positive {x @ gt10}} : Num {- x 10}}
                           {positive 15}})
          "Static contract error: invalid type for gt10")

; argumento de tipo Num
(test/exn (run-typecheck '{{define {gt10 {x : Num}} : Bool {> x 10}}
                           {define {positive {x @ gt10}} : Num {- x 10}}
                           {positive 15}})
          "Static contract error: invalid type for gt10")

; argumento de tipo Bool
(test/exn (run-typecheck '{{define {gt10 {x : Bool}} : Bool {&& x #f}}
                           {define {positive {x @ gt10}} : Num {- x 10}}
                           {positive 15}})
          "Static contract error: invalid type for gt10")

; argumento de tipo Any funciona
(test (run-typecheck '{{define {gt10 {x : Any}} : Bool {> x 10}}
                       {define {positive {x @ gt10}} : Num {- x 10}}
                       {positive 15}})
      5)

; argumento sin tipo definido funciona
(test (run-typecheck '{{define {gt10 x} : Bool {> x 10}}
                       {define {positive {x @ gt10}} : Num {- x 10}}
                       {positive 15}})
      5)

; Contract broken in runtime
(test/exn (run-typecheck '{{define {gt10 x} : Bool {> x 10}}
         {define {positive {x @ gt10}} : Num {- x 10}}
         {positive 5}})
          "Runtime contract error: x does not satisfy gt10")

; Contract broken in runtime
(test/exn (run-typecheck '{{define {add x y} : Num {+ x y}}
                           {define {oh-no {x @ add} y}
                            #t}
                           {oh-no 21 21}})
          "Static contract error: invalid type for add")

(test (run-typecheck '{{define {positive x} : Bool {> x 0}}
                       {define {div {x : Num @ positive} y}
                         {/ y x}}
                       {div 5 3}})
      3/5)

(test (run-typecheck '{{define {lt100 x} {< x 100}}
                       {define {positive x} : Bool {> x 0}}
                       {define {percentage? x} : Bool {&& {lt100 x} {positive x}}}
                       {define {calc {x @ positive} {y @ percentage?}}
                         {/ {* y y} x}}
                       {calc 25 3}})
      9/25)