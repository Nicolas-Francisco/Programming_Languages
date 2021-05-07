#lang play

;----------------------------------------------------------------------------------;
#|
                        CC4101 - LENGUAJES DE PROGRAMACIÓN
                                     TAREA 1

                             Profesor: Éric Tanter
                     Auxiliares: Bryan Ortiz - Tomas Vallejos
                         Estudiante: Nicolás García Ríos
|#
;----------------------------------------------------------------------------------;

; <Prog> ::= {<Fundef>* <Expr>}
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

; unops :: List[Unop]
(define unops (list '! 'add1 'sub1))

; is-unop? :: sym -> boolean
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
    ['> >]
    ['&& (λ (x y) (and x y))]
    ['|| (λ (x y) (or x y))]))

; unops : List[Binpp]
(define binops (list '+ '- '* '/ '= '< '> '&& '||))

; is-binop? :: sym -> boolean
(define (is-binop? x) (member x binops))


; parse :: scr -> Expr | FunDef
; Parser for the Expr data structure
(define (parse src)
  (match src
    [(? number?) (num src)]
    [(? symbol?) (id src)]
    [(? boolean?) (bool src)]
    [(list (? is-unop? op) expr) (unop (parse-unop op) (parse expr))]
    [(list (? is-binop? op) l r) (binop (parse-binop op) (parse l) (parse r))]
    ; if src is an unary or binary operator -> we call the parse-unop/binop
    ; if src is $$ or ||, we define the operator with a λ expression
    [(list 'if c t f) (if-expr (parse c) (parse t) (parse f))]
    [(list 'with id-list b) (with (map parse-with id-list) (parse b))]
    ; if src is a with expression, we apply a specific parse on the id-list called
    ; parse-cons. This parse returns the first element of the pair *raw* (tha same
    ; received from src) and NOT in "((id x) (num n))" form, but in
    ; "('x (num n))" form instead.
    [(list 'define args b) (fundef (first args) (rest args) (parse b))]
    ; if it's a define expresion, we take the first element as the name of the
    ; new function, the rest of the list as the arguments, and b as the body
    [(list fname ...) (app (first src) (map parse (rest src)))]
    ; if it's an id followed with Expr*, then it is a function aplication, and
    ; the rest of the src must be parsed.
    ))


; parse-cons :: List[(src src)] -> List[(src Expr)]
; This parse takes a pair of elements and returns the same pair, with the first
; element in src form and the second in a Expr form.
(define (parse-with pair)
  (list (first pair) (parse (second pair))))

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


; lookup-fundef :: Sym x Listof[FunDef] -> FunDef?
; It receives the name of a function and a list of FunDefs. If there exists a function
; with the given name, it returns the definition, if not, it throws an error.
(define (lookup-fundef f funs)
  (match funs
    ['() (error 'lookup-fundef "function not found: ~a" f)]
    [(cons (and fd (fundef fn _ _)) rest)
     (if (symbol=? fn f)
         fd
         (lookup-fundef f rest))]))


; interp :: Expr x List[FunDef] x Env -> Val?
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
    ; If the expresion is a unary or binary operation, we must verify the types of
    ; the received args and report an error if the operation receives a type error.
    [(if-expr c tb fb) (if (equal? #t (interp c fundefs env))
                           (interp tb fundefs env)
                           (interp fb fundefs env))]
    [(with id-list b) (interp b fundefs (extend-env-list id-list fundefs env))]
    ; If we are in the with case, we must extend the enviroment recursively using
    ; the id-list. To do so, we call a second enviroment extender function but
    ; applied to this specific case (list of pairs "((id 'x) (num n))" ).
    [(app fname e)
     (def (fundef _ args body) (lookup-fundef fname fundefs))
     (interp body
             fundefs
             (extend-env-pair args
                              e 
                              fundefs
                              env
                              empty-env))]
    ; If we are in the app case, we must look for the function in the fundef list,
    ; then use interp recursively on the body of the definition founded by the
    ; look-up, and then use the enviroment to substitute the arguments.
    ; To do so, we have to extend an empty environment that will be the enviroment
    ; for the function only (lexic scope) calling a third environment extender
    ; function but applied to this specific case (args as values for the ids of
    ; the function def.).
    ))


; extend-env-list :: List[(id num)] x List[FunDefs] x Env -> Env
; Extends an enviroment recursively, when it receives a long list (with case).
(define (extend-env-list id-list fundefs env)
  (match id-list
    ['() env]
    [(? list?) (extend-env-list (rest id-list)
                                fundefs
                                (extend-env (first (first id-list))
                                            (interp (second (first id-list))
                                                    fundefs
                                                    env)
                                            env))]))


; extend-env-pair :: List[(id num)] x List[FunDefs] x Env -> Env
; Extends an enviroment recursively, when it receives a long list (with case).
(define (extend-env-pair args expr fundefs main-env fun-env)
  (match args
  ['() fun-env]
  [(? list?) (extend-env-pair (rest args)
                              (rest expr)
                              fundefs
                              main-env
                              (extend-env (first args)
                                          (interp (first expr)
                                                               fundefs
                                                               main-env)
                                          fun-env))]))


; run :: src -> Val?;
; Runs a program received from the console in src form
; To do so, we first have to generate the Program in a Prog structure, and then
; use the interpreter on the parsed expresion. 
(define (run src)
  (def (program fundefs expr) (prog-parse (map parse src)))
  (interp expr fundefs empty-env))


; prog-src :: List[Expr] x List[FunDef]* x emExpr -> Prog
; This function generates a program from a list of expresions and functions.
(define (prog-parse list-expr [fundefs '()] [expr empty])
  (if (eq? list-expr '())
      (program fundefs expr)
      (if (fundef? (first list-expr))
          (prog-parse (rest list-expr) (append fundefs (list(first list-expr))) expr)
          (prog-parse (rest list-expr) fundefs (first list-expr)))))
