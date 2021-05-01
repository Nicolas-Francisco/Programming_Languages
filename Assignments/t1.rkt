#lang play

; <Prog>   ::= {<Fundef>* <Expr>}
; Program data structure
; A program is composed of 0 or more FunDefs and a expression.
; fedfs is a pair, which can contain fundefs or an empty pair.
(deftype Prog
  (program fdefs expr))


; <FunDef> ::= {define {<id> <id>*} <Expr>}
; Function definition data structure.
; A function is composed of its name, 0 or more ids as argument and a expression
(deftype FunDef
  (fundef fname args expr))


#|-----------------------------
Expression abstract data type

representation BNF:
<Expr>   ::= <num>
           | <id>
           | <bool>           
           | {<unop> <Expr>}
           | {<binop> <Expr> <Expr>}
           | {if <Expr> <Expr> <Expr>}
           | {with {{<id> <Expr>}*} <Expr>}
           | {<id> <Expr>*}
|#
(deftype Expr
  (num n)
  (id x)
  (bool b)
  (unop op expr)
  (binop op l r)
  (if-expr c t-branch f-branch)
  (with id-list body)
  (app fname args))


#|-----------------------------
Environment abstract data type
 
empty-env  :: Env
extend-env :: Sym Val Env -> Env
env-lookup :: Sym Env -> Val
 
representation BNF:
<env> ::= (mtEnv)
        | (aEnv <id> <val> <env>)
|#
(deftype Env
  (mtEnv)
  (aEnv id val rest-env))
 
(def empty-env  (mtEnv))
 
(def extend-env aEnv)
 
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest)
     (if (symbol=? id x)
         val
         (env-lookup x rest))]))


; <Unop>   ::= ! | add1 | sub1
; We can make parse for the unary op.
(define (parse-unop un)
  (match un
    ['! not]
    ['add1 add1]
    ['sub1 sub1]))

(define unops (list '! 'add1 'sub1))
(define (is-unop? x) (member x unops))

; <Binop>  ::= + | - | * | / | && | = | < | ...
; && and || can be defined with a λ expression directly from the parse
(define (parse-binop bin)
  (match bin
    ['+ +]
    ['- -]
    ['* *]
    ['/ /]
    ['= =]
    ['< <]
    ['> >]))

(define binops (list '+ '- '* '/ '= '< '>))
(define (is-binop? x) (member x binops))

; parse ::= scr -> Expr
; Parser for the Expr data structure
(define (parse src)
  (match src
    [(? number?) (num src)]
    [(? symbol?) (id src)]
    [(? boolean?) (bool src)]
    [(list (? is-unop? op) expr) (unop (parse-unop op) (parse expr))]
    [(list (? is-binop? op) l r) (binop (parse-binop op) (parse l) (parse r))]
    [(list '&& l r) (binop (λ (x y) (and x y)) (parse l) (parse r))]
    [(list '|| l r) (binop (λ (x y) (or x y)) (parse l) (parse r))]
    ; if src is an unary or binary operator -> we call the parse-unop/binop
    ; if src is $$ or ||, we define the operator with a λ expression
    [(list 'if c t f) (if-expr (parse c) (parse t) (parse f))]
    [(list 'with id-list b) (with (map parse-cons id-list) (parse b))]
    ; if src is a with expression, we apply a specific parse on the id-list called
    ; parse-cons. This parse returns the first element of the pair *raw* (as received
    ; from the src) and NOT in "((id x) (num n))" form, but in "('x (num n))" instead.
    [(list 'define args b) (fundef (first args) (rest args) (parse b))]
    ; if it's a define expresion, we take the first element as the name of the
    ; new function, the rest of the list as the arguments, and b as the body
    [(list fname args ...) (app fname (map parse (rest src)))]
    ; if it's an id followed with Expr*, then it is a function aplication, and the
    ; rest of the src must be parsed.
    ))

; parse-cons ::= list -> list
; This parse takes a pair of elements and returns the same pair, with the first
; element in src form and the second in a Expr form.
(define (parse-cons pair)
  (list (first pair) (parse (second pair))))

