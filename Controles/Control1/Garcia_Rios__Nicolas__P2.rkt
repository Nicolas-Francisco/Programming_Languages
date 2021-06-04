#lang play
;; base p2
(print-only-errors)

(deftype Fundef
  (fun name args body))

(deftype Expr
  (num n)
  (id x)
  (add l r)
  (if0 c t f)
  (app f args))

#|
<fundef> ::= {define {<id> <id>*} <expr>}
|#
(define (parse-fundef fd)
  (match fd
    [(list 'define (list id args ...) body)
     (fun id args (parse body))]
    [e (error "error parsing fundef")]))

#|
<expr>  ::= <num>
         |  <id>
         |  {+ <expr> <expr>}
         |  {if0 <expr> <expr> <expr>}
         |  {<id> <expr>*}
|#
(define (parse s-expr)
  (match s-expr
    [(? number? n) (num n)]
    [(? symbol? x) (id x)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list 'if c t f) (if0 (parse c) (parse t) (parse f))]
    [(list f a ...) (app f (map parse a))]
    [e (error "error parsing expr")]))

(test (parse '5) (num 5))
(test (parse 'x) (id 'x))
(test (parse '{+ 37 5}) (add (num 37) (num 5)))
(test (parse '{if 0 1 42}) (if0 (num 0) (num 1) (num 42)))
(test (parse '{f 123}) (app 'f (list (num 123))))
(test (parse '{f 123 12 23}) (app 'f (list (num 123) (num 12) (num 23))))
(define bdy '{if x {+ y z} 30})
(test (parse-fundef `{define {f x y z} ,bdy}) (fun 'f (list 'x 'y 'z) (parse bdy)))

;; is-tail-recursive :: Fundef -> boolean
; A program that checks if some function definition is tail-recursive
(define (is-tail-recursive function)
  (def body (fun-body function))
  (def name (fun-name function))
  (check-tail-recursive name body))

; check-tail-recursive :: sym expr -> boolean
; this function checks if some expresion is tail-recursive.
(define (check-tail-recursive name body)
  (match body
    [(num n) #f]
    [(id n) #f]
    [(add l r) #f]
    [(if0 c t f)
     (if (check-tail-recursive name c)
          #f
          (or (check-tail-recursive name t) (check-tail-recursive name f)))]
    [(app f args) (if (eq? name f) #t #f)]))

; run :: src -> boolean
(define (run-tr prog)
  (is-tail-recursive (parse-fundef prog)))

(test (run-tr '{define {fact n} {if n
                                1
                                {+ n {fact {+ n 1}}}}})
      #f)

(test (run-tr '{define {fact n acc} {if n
                                    acc
                                    {fact {+ n 1} {+ n acc}}}})
      #t)