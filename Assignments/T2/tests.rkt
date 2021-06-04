#lang play
(require "main.rkt")
(print-only-errors #t)

;; Test sub-module.
;; See http://blog.racket-lang.org/2012/06/submodules.html

;this tests should never fail; these are tests for MiniScheme+
(module+ test
  (test (run '{+ 1 1}) 2)

  (test (run '{{fun {x y z} {+ x y z}} 1 2 3}) 6)

  (test (run '{< 1 2}) #t)

  (test (run '{local {{define x 1}}
                x}) 1)

  (test (run '{local {{define x 2}
                      {define y {local {{define x 1}} x}}}
                {+ x x}}) 4)

  ;; datatypes
  (test (run '{local {{datatype List {Empty} {Cons a b}}} {List? {Empty}}}) #t)

  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Empty? {Empty}}}) #t)

  (test (run '{local {{datatype List {Empty} {Cons a b}}} {List? {Cons 1 2}}}) #t)

  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Cons? {Cons 1 2}}}) #t)

  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Empty? {Cons 1 2}}})
        #f)

  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Empty? {Empty}}}) #t)

  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Cons? {Empty}}})
        #f)

  ;; match
  (test (run '{match 1 {case 1 => 2}}) 2)

  (test (run '{match 2
                {case 1 => 2}
                {case 2 => 3}})
        3)

  (test (run '{match #t {case #t => 2}}) 2)

  (test (run '{match #f
                {case #t => 2}
                {case #f => 3}})
        3)

  (test (run '{local {{datatype Nat
                                {Zero}
                                {Succ n}}
                      {define pred {fun {n}
                                        {match n
                                          {case {Zero} => {Zero}}
                                          {case {Succ m} => m}}}}}
                {Succ? {pred {Succ {Succ {Zero}}}}}})
        #t)
  (test (run '{local {{datatype Nat
                                {Zero}
                                {Succ n}}
                      {define pred {fun {n}
                                        {match n
                                          {case {Zero} => {Zero}}
                                          {case {Succ m} => m}}}}}
                {Succ? {pred {Succ {Succ {Zero}}}}}}) #t))

;tests for extended MiniScheme+
;uncomment sanity tests when you are ready
#;(module+ sanity-tests
    (test (run '{local {{datatype Nat
                  {Zero}
                  {Succ n}}
                {define pred {fun {n}
                               {match n
                                 {case {Zero} => {Zero}}
                                 {case {Succ m} => m}}}}}
          {pred {Succ {Succ {Zero}}}}} "ppwu") "{Succ {Zero}}")

(test (run
 `{local ,stream-lib
          {local {,ones ,stream-take}
            {stream-take 11 ones}}} "pp") "{list 1 1 1 1 1 1 1 1 1 1 1}")

(test (run `{local ,stream-lib
          {local {,stream-zipWith ,fibs}
            {stream-take 10 fibs}}} "pp") "{list 1 1 2 3 5 8 13 21 34 55}")

(test (run `{local ,stream-lib
          {local {,ones ,stream-zipWith}
            {stream-take 10
                         {stream-zipWith
                          {fun {n m}
                               {+ n m}}
                          ones
                          ones}}}} "pp")  "{list 2 2 2 2 2 2 2 2 2 2}")
(test
(run `{local ,stream-lib
               {local {,stream-take ,merge-sort ,fibs ,stream-zipWith}
                 {stream-take 10 {merge-sort fibs fibs}}}} "pp")   "{list 1 1 1 1 2 2 3 3 5 5}"))

;--------------------------------- Tests Warm-up ---------------------------------;
; test de enunciado 1
(test (pretty-printing
       (run '{local {{datatype Nat 
                  {Zero} 
                  {Succ n}}
                {define pred {fun {n} 
                               {match n
                                 {case {Zero} => {Zero}}
                                 {case {Succ m} => m}}}}}
          {pred {Succ {Succ {Zero}}}}}))
      "{Succ {Zero}}")

; test de enunciado 2
(test (pretty-printing
       (run '{local {{datatype Nat 
                  {Zero} 
                  {Succ n}}
                {define twice {fun {n} 
                               {match n
                                 {case {Zero} => {Zero}}
                                 {case {Succ m} => {Succ {Succ {twice m}}}}}}}}
          {twice {Succ {Succ {Zero}}}}}))
      "{Succ {Succ {Succ {Succ {Zero}}}}}")

; test de enunciado 3 - ejemplo de uso de pretty-printing
(test (pretty-printing (structV 'Nat 'Succ (list (structV 'Nat 'Zero empty))))
      "{Succ {Zero}}")

; test de enunciado 4 - uso de flag "ppwu"
(test (run '{local {{datatype Nat 
                  {Zero} 
                  {Succ n}}
                {define pred {fun {n} 
                               {match n
                                 {case {Zero} => {Zero}}
                                 {case {Succ m} => m}}}}}
          {pred {Succ {Succ {Succ {Zero}}}}}} "ppwu")
      "{Succ {Succ {Zero}}}")

; test 5 - estructura con más de un elemento
(test (run '{local {{datatype Arbol 
                  {Leaf} 
                  {Node a b}}}
          {Node {Leaf} {Node {Leaf} {Leaf}}}} "ppwu")
      "{Node {Leaf}{Node {Leaf}{Leaf}}}")

; test 6 - estructura con valores numéricos
(test (run '{local {{datatype Arbol 
                  {Leaf v} 
                  {Node a b}}}
          {Node {Leaf 1} {Node {Leaf #t} {Leaf "hello"}}}} "ppwu")
      "{Node {Leaf 1}{Node {Leaf #t}{Leaf hello}}}")

; test 7 - estructura list con flag "ppwu"
(test (run '{local {{datatype List 
                  {Empty} 
                  {Cons a b}}}
          {Cons 1 {Cons #t {Cons "hello" {Empty}}}}} "ppwu")
      "{Cons 1{Cons #t{Cons hello{Empty}}}}")

;--------------------------------- Tests listas ---------------------------------;
; test de enunciado 1
(test (run '{Empty? {Empty}})
      #t)

; test de enunciado 2
(test (run '{List? {Cons 1 2}})
      #t)

; test de enunciado 3
(test (run '{length {Cons 1 {Cons 2 {Cons 3 {Empty}}}}})
      3)

; test de enunciado 4 - azucar sintáctico 1
(test (run '{match {list {+ 1 1} 4 6}
          {case {Cons h r} => h}
          {case _ => 0}})
      2)

; test de enunciado 5 - azucar sintáctico 2
(test (run '{match {list}
          {case {Cons h r} => h}
          {case _ => 0}})
      0)

; test de enunciado 6 - extend del patter matching
(test (run '{match {list 2 {list 4 5} 6}
          {case {list a {list b c} d} => c}})
      5)

; test de enunciado 7 - flag pp 1
(test (run '{list 1 4 6} "pp")
      "{list 1 4 6}")

; test de enunciado 8 - flag pp 2
(test (run '{list} "pp")
      "{list}")

;--------------------------------- Tests lazyness ---------------------------------;
; test de enunciado 1
(test (run '{{fun {x  {lazy y}} x} 1 {/ 1 0}})
      1)

; test de enunciado 2
(test (run '{local {{datatype T 
                  {C {lazy a}}}
                {define x {C {/ 1 0}}}}
          {T? x}})
      #t)