#lang play
(print-only-errors)
; base p3

(deftype Expr
  (num n)
  (add l r)
  (sub l r) ; Añadimos el sub por el ejemplo de DeBruijn
  (id x)
  (with x n e)
  (withDB n e)) ; Creamos el with de DeBruijn

#|
a) Teniendo la siguiente expresion
(with (x 5)
      (with (y (+ x 2))
            (+ x (with (y (- y 1))
                       (+ y x)))))

Podemos convertirla usando índices de DeBruijn como:

(with 5
      (with (+ <0> 2)
            (+ <1> (with (- <0> 1)
                         (+ <0> <2>)))))
|#



#|
b)
<expr> ::= <id>
        | <num>
        | {+ <expr> <expr>}
        | {- <expr> <expr>}
        | {with {<id> <expr>} <expr>}
        | {withDB <expr> <expr>}
|#
(define (parse s-expr)
  (match s-expr
    [(? number? n) (num n)]
    [(? symbol? x) (id x)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list bid ne) b)
     (with bid (parse ne) (parse b))]
    ; Creamos el caso de una expresion de DeBruijn (de la forma named
    ; expresion y el body, y no lista de binding, named expresion y body).
    [(list 'with ne b) (withDB (parse ne) (parse b))]
    [e (error "error parsing expr")]))

(test (parse '{with {x 5} {+ x 3}}) (with 'x (num 5) (add (id 'x) (num 3))))

; Ejemplo de la P3a)
(test (parse '{with 5
                    {with {+ <0> 2}
                          {+ <1> {with {+ <0> 1}
                                       {+ <0> <2>}}}}})
      (withDB (num 5)
              (withDB (add (id '<0>) (num 2))
                      (add (id '<1>) (withDB (add (id '<0>) (num 1))
                                             (add (id '<0>) (id '<2>)))))))

;c)

;; toDeBruijn :: Expr -> Expr
; Toma una expresion parseada y la transforma a una expresion de DeBruijn
; Usaremos ambientes para trabajar los ids
(define (toDeBruijn expr)
  (toDeBruijn-expr expr (mtEnv)))

;; toDeBruijn-expr :: Expr Env -> Expr
; Esta función transforma una expresión a expresión de DeBruijn de forma recursiva.
; Extenderemos el ambiente cada vez que encontremos un with normal con el label,
; y lo convertiremos a un with de DeBruijn.
(define (toDeBruijn-expr expr env)
  (match expr
    ; Si es num, lo retornamos de vuelta
    [(num n) expr]
    ; Si es un id, debemos generar el simbolo de DeBruijn con el label que se
    ; encuentre en el ambiente
    [(id x) (id (generateDB x env))]
    ; para la suma y resta basta hacer recursion
    [(add l r) (add (toDeBruijn-expr l env) (toDeBruijn-expr r env))]
    [(sub l r) (sub (toDeBruijn-expr l env) (toDeBruijn-expr r env))]
    ; caso with, lo transformamos a DeBruijn y extendemos el ambiente
    [(with bid ne b)
     (withDB (toDeBruijn-expr ne env) (toDeBruijn-expr b (extend-env bid env)))]
    ; caso withDB, lo retornamos de vuelta
    [(withDB ne b) expr]))

;; generateDB :: sym Env -> sym
; Genera el simbolo que usaremos en expresion de DeBruijn
(define (generateDB id env)
  (string->symbol (string-append "<" (~v (env-lookup id env 0)) ">")))

#|-----------------------------
Environment abstract data type
 
empty-env  :: Env
extend-env :: Sym Env -> Env
env-lookup :: Sym Env num -> Val

We can extend the previously known Environment in order to
store the types of the ids.

representation BNF:
<env> ::= (mtEnv)
        | (aEnv <id> <env>)
|#

; Utilizaremos el mismo ambiente visto en clases, pero en vez de retornar un
; valor, retornaremos el label con el que generamos DeBruijn.
(deftype Env
  (mtEnv)
  (aEnv id env))

(define empty-env mtEnv)

(define extend-env aEnv)

(define (env-lookup x env label)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id rest)
     (if (eq? id x)
         label
         (env-lookup x rest (add1 label)))]))

; test enunciado
(test (toDeBruijn (parse '{with {x 5} {+ x x}}))
      (withDB (num 5) (add (id '<0>) (id '<0>))))

; test de la p1a
(test (toDeBruijn (with 'x (num 5)
                        (with 'y (add (id 'x) (num 2))
                              (add (id 'x)
                                   (with 'y (sub (id 'y) (num 1))
                                         (add (id 'y) (id 'x)))))))
      (withDB (num 5)
              (withDB (add (id '<0>) (num 2))
                      (add (id '<1>) (withDB (sub (id '<0>) (num 1))
                                             (add (id '<0>) (id '<2>)))))))