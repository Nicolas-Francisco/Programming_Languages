#lang play
(print-only-errors #t)

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


; parse ::= scr -> Expr | FunDef
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
    [(list fname ...) (app (first src) (map parse (rest src)))]
    ; if it's an id followed with Expr*, then it is a function aplication, and the
    ; rest of the src must be parsed.
    ))


; parse-cons ::= list -> list
; This parse takes a pair of elements and returns the same pair, with the first
; element in src form and the second in a Expr form.
(define (parse-cons pair)
  (list (first pair) (parse (second pair))))

;------------------------------------ PARSER TESTS ------------------------------------;
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
      (with (list (list 'x (num 5)) (list 'y (num 7)) (list 'z (num 42)))
            (id 'z)))
; define case
(test (parse '{define {sum3 x y z} {+ {+ x y} z}})
      (fundef 'sum3 '(x y z) (binop + (binop + (id 'x) (id 'y)) (id 'z))))
; app case
(test (parse '{sum3 1 2 3})
      (app 'sum3 (list (num 1) (num 2) (num 3))))
;------------------------------------ PARSER TESTS ------------------------------------;

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


; lookup-fundef :: sym Listof(FunDef) -> FunDef?
; It receives the name of a function and a list of FunDefs. If there exists a function
; with the given name, it returns the definition, if not, it throws an error.
(define (lookup-fundef f funs)
  (match funs
    ['() (error 'lookup-fundef "function not found: ~a" f)]
    [(cons (and fd (fundef fn _ _)) rest)
     (if (symbol=? fn f)
         fd
         (lookup-fundef f rest))]))


; interp ::= Expr -> Val?
; Interpreter for the Expr data structure.
; It receives the expresion, a list of fundefs and a enviroment.
(define (interp expr fundefs env)
  (match expr
    [(num n) n]
    [(bool b) b]
    [(id x) (env-lookup x env)]
    ; If the expression is an id, we look for it in the environment.
    [(unop op e) (op (interp e fundefs env))]
    [(binop op l r) (op (interp l fundefs env) (interp r fundefs env))]
    ; PART 2:
    ; If the expresion is a unary or binary operation, we must verify the types of the
    ; received args and report an error if the operation receives a type error.
    [(if-expr c tb fb) (if (boolean? (interp c fundefs env))
                           (if (equal? #t (interp c fundefs env))
                               (interp tb fundefs env)
                               (interp fb fundefs env))
                           (error "Runtime type error: expected Boolean"))]
    [(with id-list b) (interp b fundefs (extend-env (first id-list) fundefs env))]
    [(app fname args)
     (def (fundef _ farg fbody) (lookup-fundef fname fundefs))
     (interp fbody
             fundefs
             (extend-env farg
                         (interp args fundefs env)
                         empty-env))]))

;------------------------------------ INTERP TESTS ------------------------------------;
; num case
(test (interp (num 1) '() empty-env)
      1)
; bool case
(test (interp (bool #t) '() empty-env)
      #t)
; id case
(test/exn (interp (id 'x) '() empty-env)
      "free identifier: x")
(test (interp (id 'x) '() (aEnv 'x 1 empty-env))
      1)
; unop case
(test (interp (unop not (bool #t)) '() empty-env)
      #f)
; binop case
(test (interp (binop + (num 1) (num 2)) '() empty-env)
      3)
; if case
(test (interp (if #t (num 1) (num 2)) '() empty-env)
      1)
(test (interp (if #f (num 1) (num 2)) '() empty-env)
      2)
; with case
(test (interp (with (list (list 'x 2) (list 'y 2)) (binop + (id 'x) (id 'y))) '() empty-env)
      3)
;------------------------------------ PARSER TESTS ------------------------------------;

; run ::= src -> Val?
(define (run prog [funs '()])
  (interp (parse prog) funs empty-env))