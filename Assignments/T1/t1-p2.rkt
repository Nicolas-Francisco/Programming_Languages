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


; <FunDef> ::= {define {<id> <arg>*} [<type>] <Expr>}
; Function definition data structure.
; A function is composed of its name, 0 or more ids as argument and a expression
(deftype FunDef
  (fundef fname args type expr))


; <Arg> ::= <id> | {<id> : <Type>}
; Argument data structure.
; An argument can be a simple id or an id with type annotation
(deftype Arg
  (arg-any id)
  (arg-type id type))


; <Type> :: Num | Bool | Any
(deftype Type
  (Num)
  (Bool)
  (Any))


; parse-type :: src -> Type
; A parser for the type received.
(define (parse-type type)
  (match type
    ['Num Num]
    ['Bool Bool]
    ['Any Any]))


#|-----------------------------
Expression abstract data type

representation BNF:
<Expr>   ::= <num>
           | <id>
           | <bool>           
           | {<unop> <Expr>}
           | {<binop> <Expr> <Expr>}
           | {if <Expr> <Expr> <Expr>}
           | {with {{<id> [: <type>] <Expr>}*} <Expr>}
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
    ; parse-args that matches the type received (or not) with the real type.
    [(list 'define args ': type body) (fundef (first args)
                                              (map parse-args (rest args))
                                              (parse-type type)
                                              (parse body))]
    [(list 'define args body) (fundef (first args)
                                      (map parse-args (rest args))
                                      (parse-type 'Any)
                                      (parse body))]
    ; if it's a define expresion, we take the first element as the name of the
    ; new function, the rest of the list as the arguments with it's own types,
    ; and two different cases dependig if in the define we are specifing the type.
    ; If we have [: src], then we parse the type and give it to the fundef, if it
    ; does not have [: src], we use parse the Any type.
    [(list fname args ...) (app (first src) (map parse (rest src)))]
    ; if it's an id followed with Expr*, then it is a function aplication, and
    ; the rest of the src must be parsed.
    ))


; parse-with :: List[(src [: src] src)] -> List[(src Type Expr)]
; This parse takes a list of elements and returns the same list, with the first
; element in src form, the second as a Type form (Any by defect) and the third
; element as the value of the id.
; The function returns three values in src, type and expr form.
(define (parse-with id-list)
  (match id-list
    ['() '()]
    [(list id ': type expr) (list id (parse-type type) (parse expr))]
    [(list id expr) (list id (parse-type 'Any) (parse expr))]))


; parse-args :: List[(src Type)] -> Arg
; This parse takes a list of arguments from the function definition and transforms
; it into a Argument data structure
(define (parse-args args)
  (if (list? args)
      (arg-type (first args) (parse-type (third args)))
      (arg-any args)))


#|-----------------------------
Environment abstract data type
 
empty-env  :: Env
extend-env :: Sym Type Val Env -> Env
env-lookup :: Sym Env -> Val
env-lookup-type :: Sym Env -> Type
 
representation BNF:
<env> ::= (mtEnv)
        | (aEnv <id> <type> <val> <env>)
|#
(deftype Env
  (mtEnv)
  (aEnv id type val rest-env))
 
(def empty-env (mtEnv))
 
(def extend-env aEnv)
 
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id type val rest)
     (if (symbol=? id x)
         val
         (env-lookup x rest))]))

(define (env-lookup-type x env)
  (match env
    [(mtEnv) (error 'env-lookup-type "free identifier: ~a" x)]
    [(aEnv id type val rest)
     (if (symbol=? id x)
         type
         (env-lookup-type x rest))]))


; lookup-fundef :: Sym x Listof[FunDef] -> FunDef?
; It receives the name of a function and a list of FunDefs. If there exists a function
; with the given name, it returns the definition, if not, it throws an error.
(define (lookup-fundef f funs)
  (match funs
    ['() (error 'lookup-fundef "function not found: ~a" f)]
    [(cons (and fd (fundef fn _ _ _)) rest)
     (if (symbol=? fn f)
         fd
         (lookup-fundef f rest))]))


; num-op? :: <Unop> | <Binop> -> boolean
(define num-op (list add1 sub1 + - * / < > =))
(define (num-op? op) (member op num-op))


; verify-unop :: Unop x Expr x List[FunDef] x Env -> Val? 
; This function checks if the type of the received expression is compatible
(define (verify-unop op expr fundefs env)
  (let ([e (interp expr fundefs env)])
  (if (num-op? op)
      (if (number? e)
          (op e) 
          (error "Runtime type error: expected Number found Boolean"))
      (if (boolean? e)
          (op e) 
          (error "Runtime type error: expected Boolean found Number")))))


; verify-binop :: Binop x Expr x List[FunDef] x Env -> Val? 
; This function checks if the type of the received expression is compatible
(define (verify-binop op left right fundefs env)
  (let ([l (interp left fundefs env)]
        [r (interp right fundefs env)])
  (if (num-op? op)
      (if (and (number? l) (number? r))
          (op l r) 
          (error "Runtime type error: expected Number found Boolean"))
      (if (and (boolean? l) (boolean? r))
          (op l r) 
          (error "Runtime type error: expected Boolean found Number")))))


; interp :: Expr x List[FunDef] x Env -> Val?
; Interpreter for the Expr data structure.
; It receives the expresion, a list of fundefs and a enviroment.
(define (interp expr fundefs env)
  (match expr
    [(num n) n]
    [(bool b) b]
    [(id x) (env-lookup x env)]
    ; If the expression is an id, we look for it in the environment.
    [(unop op e) (verify-unop op e fundefs env)]
    [(binop op l r) (verify-binop op l r fundefs env)]
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
     (def (fundef _ args _ body) (lookup-fundef fname fundefs))
     (interp body
             fundefs
             (extend-env-app args
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


; extend-env-list :: List[(id type num)] x List[FunDefs] x Env -> Env
; Extends an enviroment recursively, when it receives a long list (with case).
(define (extend-env-list id-list fundefs env)
  (match id-list
    ['() env]
    [(? list?) (extend-env-list (rest id-list)
                                fundefs
                                (extend-env (first (first id-list))
                                            (second (first id-list))
                                            (interp (third (first id-list))
                                                    fundefs
                                                    env)
                                            env))]))


; extend-env-app :: List[(id type num)] x List[FunDefs] x Env -> Env
; Extends an enviroment recursively, when it receives a long list (with case).
(define (extend-env-app args expr fundefs main-env fun-env)
  (match args
  ['() fun-env]
  [(? list?) (if (arg-type? (first args))
                 (extend-env-app (rest args)
                                 (rest expr)
                                 fundefs
                                 main-env
                                 (extend-env (arg-type-id (first args))
                                             (arg-type-type (first args))
                                             (interp (first expr) fundefs main-env)
                                             main-env))
                 (extend-env-app (rest args)
                                 (rest expr)
                                 fundefs
                                 main-env
                                 (extend-env (arg-any-id (first args))
                                             Any
                                             (interp (first expr) fundefs main-env)
                                             main-env)))]))


; num-bool-op? :: <Binop> -> boolean
; operators that receive numbers but return a boolean
(define num-bool-ops (list < > =))
(define (is-num-bool-op? op) (member op num-bool-ops))


; typeof :: Expr -> Type?
; checks the type of an expression
(define (typeof expr fundefs env)
  (match expr
    [(num _) Num]
    [(bool _) Bool]
    [(id x) (env-lookup-type x env)]
    ; If the expression is an id, we look for it in the environment.
    [(unop op expr) (if (num-op? op)
                        (let ([tl (typeof expr fundefs env)])
                          (if (equal? Bool tl)
                              (error "Static type error: expected Num found Bool")
                              Num))
                        (let ([tl (typeof expr fundefs env)])
                          (if (equal? Num tl)
                              (error "Static type error: expected Bool found Num")
                              Bool)))]
    [(binop op l r) (if (num-op? op)
                        (let ([tl (typeof l fundefs env)]
                              [tr (typeof r fundefs env)])
                          (if (or (equal? Bool tl) (equal? Bool tr))
                              (error "Static type error: expected Num found Bool")
                              (if (is-num-bool-op? op)
                                  Bool
                                  Num)))
                        (let ([tl (typeof l fundefs env)]
                              [tr (typeof r fundefs env)])
                          (if (or (equal? Num tl) (equal? Num tr))
                              (error "Static type error: expected Bool found Num")
                              Bool)))]
    ; If the expresion is a unary or binary operation, we must verify the types of
    ; the received args and report an error if the operation receives a type error.
    [(if-expr c tb fb) (if (equal? Bool (typeof c fundefs env))
                           (let ([tt (typeof tb fundefs env)]
                                 [tf (typeof fb fundefs env)])
                             (if (or (and (equal? Bool tt)
                                          (equal? Num tf))
                                     (and (equal? Num tt)
                                          (equal? Bool tf)))
                                 (error
                                  "Static type error: different types on branches")
                                 (if (or (equal? Any tt) (equal? Any tf))
                                     Any
                                     tt)))
                           (error "Static type error: expected Bool found Num"))]
    ; If it's an if expression, we must check if the condition has a Bool type, then
    ; if the types of both branches are the same, and return Any type if one of the
    ; branches has an Any type.
    [(with id-list body) (if (typeof-with id-list fundefs env)
                             (typeof body
                                     fundefs
                                     (extend-env-with id-list fundefs mtEnv))
                             (error "Static type error: id types does not match"))]
    ; If we are in the with expression, we first check if all the types of the
    ; expression received in the id-list match with the declared type. If one of
    ; arguments fails, we throw an static error.
    ; By the other hand, after checking all the ids in the list, we return the
    ; type of the body, extending a new environment with the ids.
    [(app f e) (def (fundef fname args type body) (lookup-fundef f fundefs))
               (if (typeof-app args e fundef env)
                   type
                   (error "Static type error: argument types does not match"))]
    ; If we are in the app expression, we first check if all the types of the
    ; arguments received in the id-list match with the declared type. If one of
    ; arguments fails, we throw an static error. If all the arguments match, we
    ; only have to return the type of the function.
    ))


; typeof-with :: List[(src type expr)] x List[FunDef] x Env -> boolean
; Checks if the type of every element of the id-list matches with it's expression.
(define (typeof-with list fundefs env)
  (match list
    ['() #t]
    [(cons (list id type expr) rest)
     (if (or (equal? type (typeof expr fundefs env))
             (equal? Any (typeof expr fundefs env))
             (equal? Any type))
         (typeof-with rest fundefs env)
         #f)]))


; typeof-app :: List[Expr] x  List[Expr] x List[FunDef] x Env -> boolean
; Checks if the type of every element of the argument list matches with the type
; of the expression.
(define (typeof-app args expr fundefs env)
  (if (eq? (length args) (length expr))
      (match args
        ['() #t]
        [(? list?)
         (if (arg-type? (first args))
             (let ([tl (typeof (first expr) fundefs env)])
               (if (or (equal? Any tl)
                       (equal? tl (arg-type-type (first args)))
                       (equal? Any (arg-type-type (first args))))
                   (typeof-app (rest args) (rest expr) fundefs env)
                   (if (equal? Num (arg-type-type (first args)))
                       (error "Static type error: expected Num found Bool")
                       (error "Static type error: expected Bool found Num"))))
             (typeof-app (rest args) (rest expr) fundefs env))])
      (error "Static type error: given wrong amount of arguments")))


; extend-env-with :: List[(src type expr)] x List[FunDef] x Env -> Env
; This function extends the given environment with the ids contained in the list.
(define (extend-env-with id-list fundefs env)
  (match id-list
    ['() env]
    [(? list?) (let ([te (typeof (third (first id-list)) fundefs env)])
                 (if (or (and (eq? te Num)
                              (eq? (second (first id-list)) Bool))
                         (and (eq? te Bool)
                              (eq? (second (first id-list)) Num)))
                     (error "Static type error: given wrong type of arguments")
                     (extend-env-with (rest id-list)
                                      fundefs
                                      (extend-env (first (first id-list))
                                                  (second (first id-list))
                                                  te
                                                  env))))]))


; extend-env-args :: List[Arg] x [Env] -> Env
; This function extend a temporal environment in order to check if the functions
; match the types of the body and the declared type.
(define (extend-env-args args [env mtEnv])
  (match args
    ['() env]
    [(cons arg rest) (if (arg-type? arg)
                         (extend-env-args rest
                                          (extend-env (arg-type-id arg)
                                                      (arg-type-type arg)
                                                      empty
                                                      env))
                         (extend-env-args rest
                                          (extend-env (arg-any-id arg)
                                                      Any
                                                      empty
                                                      env)))]))


; typeof-fundef :: FunDef x [List[FunDef]] x [Env] -> boolean?
; Checks if the types of the functions defined match with it's bodies.
(define (typeof-fundef fun [fundefs '()] [env mtEnv])
   (let ([tb (typeof (fundef-expr fun) fundefs (extend-env-args (fundef-args fun)))])
     (if (or (and (eq? (fundef-type fun) Num)
                  (eq? tb Bool))
             (and (eq? (fundef-type fun) Bool)
                  (eq? tb Num)))
         (error "Static type error: function type declared mismatch body type")
         #t)))


; run :: src -> Val?
; Runs a program received from the console in src form.
; To do so, we first have to generate the Program in a Prog structure, and then
; use the interpreter on the parsed expresion.
(define (run src)
  (def (program fundefs expr) (prog-parse (map parse src)))
  (interp expr fundefs mtEnv))


; prog-parse :: List[Expr] x [List[FunDef]] x [emExpr] -> Prog
; This function generates a program from a list of expresions and functions.
(define (prog-parse list-expr [fundefs '()] [expr empty])
  (match list-expr
    ['() (program fundefs expr)]
    ; If it's an empty list, we return a program immediately
    [(cons (? Expr? e) rest-expr) (prog-parse (rest list-expr)
                                              fundefs
                                              e)]
    ; If the first element of the list is a expression, then we call the prog-src
    ; recursively on the rest of the list, with an not-empty expression.
    [(? list?) (prog-parse (rest list-expr)
                           (append fundefs (list (first list-expr)))
                           expr)]
    ; If the first element of the list is NOT a expression, then we know it must
    ; be a function definition because of the parser.
    ; We just have to add this function to the function list and call prog-src
    ; recursively on the rest of the list.
    ))


; typecheck :: src -> Type?
; This functions typecheks if there is a static error in the program
(define (typecheck src)
  (def (program fundefs expr) (prog-parse (map parse src)))
  (map typeof-fundef fundefs)
  (typeof expr fundefs mtEnv))


; run-typecheck :: src -> Val?;
; Runs a program received from the console in src form using a typecheker.
; To do so, we first have to generate the Program in a Prog structure, then use
; the typecheck function to check if there is any static error, and finally it
; uses the interpreter on the parsed expresion.
(define (run-typecheck src)
  (def (program fundefs expr)(prog-parse (map parse src)))
  (let ([type (typecheck src)])
    (interp expr fundefs mtEnv)))