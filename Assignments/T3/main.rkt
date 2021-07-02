#lang play

#|
<expr> ::= <num>
         | <id>
         | <bool>
         | (if <expr> <expr> <expr>)
         | (+ <expr> <expr>)
         | '< <s-expr> <s-expr>)
         | (* <s-expr> <s-expr>)
         | (= <s-expr> <s-expr>)
         | (- <s-expr> <s-expr>)
         | (and <s-expr> <s-expr>)
         | (or <s-expr> <s-expr>)
         | (not <s-expr> <s-expr>)
         | (seqn <expr> <expr>)
         | (local { <def> ...} <expr>)

<def>    ::= (define <id> <expr>)


;EXTENSION PARA OBJETOS
<expr>  ::= ... (todo lo anterior)
         | (object [: <expr>] <member> ...)
         | this
         | (set <id> <expr>)
         | (get <id>)
         | (send <expr> <id> <expr> ...)
         | (shallow-copy <expr>)
         | (deep-copy <expr>)

<member> ::=
        | (field <id> <s-expr>)
        | (method <id> (list <id> ...) <s-expr>)

|#

(deftype Expr
  (num n)
  (bool b)
  (id s)
  (binop f l r)
  (unop f s)
  (my-if c tb fb)
  (seqn expr1 expr2)
  (lcal defs body)
  (object members) ; From here we have the extension for objects
  (this)
  (set id e)
  (get id)
  (send obj met vals)
  (shallow-copy expr)
  (deep-copy expr))

;; values
(deftype Val
  (numV n)
  (boolV b)
  (ObjectV members oenv))

;; members
(deftype Member
  (field id e)
  (method id vals body))

(deftype Def
  (my-def id expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Environment abstract data type

empty-env        :: Env
env-lookup       :: Sym Env -> Val
multi-extend-env :: List<Sym> List<Val> Env -> Env
extend-frame-env! :: Sym Val Env -> Env


representation BNF:
<env> ::= (mtEnv)
        | (aEnv <id> <val> <env>)
|#

(deftype Env
  (mtEnv)
  (aEnv hash env))

(def empty-env (mtEnv))

#|
env-lookup:: Sym Env -> Val
Busca un símbolo en el ambiente, retornando su valor asociado.
|#
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv hash rest) (hash-ref hash x (λ () (env-lookup x rest)))]))

#|
multi-extend-env:: List(Sym) List(Expr) Env -> Env
Crea un nuevo ambiente asociando los símbolos a sus valores.
|#
(define (multi-extend-env ids exprs env)
  (if (= (length ids) (length exprs))
      (aEnv (make-immutable-hash (map cons ids exprs)) env)
      (error "wrong_input, mismatched lengths")))

#|
extend-frame-env!:: Sym Val Env -> Void
Agrega un nuevo par (Sym, Val) al ambiente usando mutación.
Este método no crea un nuevo ambiente.
|#
(define (extend-frame-env! id val env)
  (match env
    [(mtEnv) (aEnv (hash id val) env)]
    [(aEnv h rEnv) (def hupd (hash-set h id val))
                   (set-aEnv-hash! env hupd)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse :: s-expr -> Expr
(define (parse s-expr)
  (match s-expr
    ['this (this)]
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(? boolean?) (bool s-expr)]
    [(list '* l r) (binop * (parse l) (parse r))]
    [(list '+ l r) (binop + (parse l) (parse r))]
    [(list '- l r) (binop - (parse l) (parse r))]
    [(list '< l r) (binop < (parse l) (parse r))]
    [(list '= l r) (binop = (parse l) (parse r))]
    [(list 'or l r) (binop (λ (i d) (or i d)) (parse l) (parse r))]
    [(list 'and l r) (binop (λ (i d) (and i d)) (parse l) (parse r))]
    [(list 'not b) (unop not (parse b))]
    [(list 'if c t f) (my-if (parse c)
                             (parse t)
                             (parse f))]
    [(list 'seqn e1 e2) (seqn (parse e1) (parse e2))]
    [(list 'local (list e ...)  b)
     (lcal (map parse-def e) (parse b))]
    ; To parse the object itself, we will use another function called parse-member.
    [(list 'object e ...) (object (map parse-member e))]  ; #f for delegation
    [(list 'set id e) (set id (parse e))] ; field
    [(list 'get id) (get id)] ; field
    [(list 'send ob met val ...)
     (send (parse ob) met (map parse val))] ; method
    [(list 'shallow-copy expr) (shallow-copy (parse expr))]
    [(list 'deep-copy expr) (deep-copy (parse expr))]
    [(list 'fun vals expr) ; #f for delegation
     (object (list (parse-member (list 'method 'f vals expr))))] 
    [(list e ...) (send (parse (first e)) 'mf (map parse (cdr e)))]))

;; parse-def :: s-expr -> Def
(define (parse-def s-expr)
  (match s-expr
    [(list 'define id b) (my-def id (parse b))]))

;; parse-member :: s-expr -> Member
; parses a member to it's corresponding type.
(define (parse-member e)
  (match e
    [(list 'field id e)
     (if (equal? 'this id)
         (error "this is a reserved word")
         (field id (parse e)))]
    [(list 'method id (list vals ...) body)
     (if (equal? 'this id)
         (error "this is a reserved word")
         (method id vals (parse body)))]))

;; interp :: Expr Env Bool/ObjectV -> Val
; We add the element inObject to determine if we are inside an object. If we are not,
; the value of inObject is false, if we are, the value is the object itself.
(define (interp expr env [inObject #f])
  (match expr
    [(num n) (numV n)]
    [(bool b) (boolV b)]
    [(binop f l r) (make-val (f (open-val (interp l env))
                                (open-val (interp r env))))]
    [(unop f s) (make-val (f (open-val (interp s env))))]
    [(my-if c t f)
     (def (boolV cnd) (interp c env))
     (if cnd
         (interp t env)
         (interp f env))]
    [(id x) (env-lookup x env)]
    [(seqn expr1 expr2) (begin
                          (interp expr1 env)
                          (interp expr2 env))]
    [(lcal defs body)
     (let ([new-env (multi-extend-env '() '() env)])
       (for-each (λ(x)
                   (def (cons id val) (interp-def x new-env))
                   (extend-frame-env! id val  new-env)
                   #t) defs)
       (interp body new-env))]
    ; We extend the interpreter with the expression
    [(object members) (ObjectV (box (interp-members members env)) env)]
    [(this) (if inObject
                (env-lookup 'self env)
                (error "not in an object"))]
    [(set x e) (if inObject
                   (interp-set x e env inObject)
                   (error "set used outside of an object"))]
    [(get id) (if inObject
                  (interp-get id inObject)
                  (error "get used outside of an object"))]
    [(send ob-id m-id vals) (interp-send ob-id m-id vals env inObject)]))

; interp-members :: List(members) aEnv [List(members)] -> List(members)
; this function takes a list of members and a enviroment, and returns a dictionary of
; the members, asociating their identifier with their meaning.
(define (interp-members members env [acum '()])
  (match members
    [(cons h t)
     (match h
       ; if we are in a field
       [(field id e) (interp-members t env (cons (cons id (interp e env)) acum))]
       ; if we are in a method
       [(method id vals body) (interp-members t env (cons (cons id (cons vals body))
                                                          acum))])]
    [empty acum]))

; interp-set :: id Expr aEnv Bool -> Expr
; this function interprets the expression in the case of an 'set'
(define (interp-set x e env inObject)
  ; we unbox the members of the object
  (let [(members (unbox (ObjectV-members inObject)))]
    ; we look up for the id in the members 
    (let [(found (assoc x members))]
      (if found
          ; if we found it, we set the box. if not, we have an error
          (let [(new-members (append (list (cons x (interp e env inObject)))
                                     members))]
            (set-box! (ObjectV-members inObject) new-members))
          (error "field not found")))))

; interp-get : id Bool -> Expr
; this function interprets the expression in the case of an 'get'
(define (interp-get id inObject)
  ; we look up for the id in the members 
  (let [(found (assoc id (unbox (ObjectV-members inObject))))]
    (if found
        (cdr found)
        (error "field not found"))))

; interp-send :: id id Expr Env Bool -> Expr
; this function interprets the expression in the case of an 'send'
(define (interp-send ob-id m-id vals env inObject)
  (begin
    (def (cons (cons id (cons args body)) fobject)
      (find-method m-id (interp ob-id env inObject)))
    (interp body
            (multi-extend-env (cons 'self args)
                              (cons (interp ob-id env inObject)
                                    (map (λ (e) (interp e env inObject)) args)))
            fobject)))

; find-method :: Symbol ObjectV -> (method ObjectV)
; this methods finds a method identifier in the list of members of the given ObjectV
; returns the method and the ObjectV where the method was found
(define (find-method m-id object)
  (match object
    [(ObjectV members oenv) (def found (assoc m-id (unbox members)))
                            (if found
                                (cons found object)
                                (find-method m-id members))]))

;; open-val :: Val -> Scheme Value
(define (open-val v)
  (match v
    [(numV n) n]
    [(boolV b) b]
    ))

;; make-val :: Scheme Value -> Val
(define (make-val v)
  (match v
    [(? number?) (numV v)]
    [(? boolean?) (boolV v)]
    ))

;; interp-def :: Def, Env -> Expr
(define (interp-def a-def env)
  (match a-def
    [(my-def id body) (cons id (interp body env))]))

;; run :: s-expr -> Val
(define (run s-expr)
  (interp (parse s-expr) empty-env))

#|
run-val:: s-expr -> Scheme-Val + Val
Versión alternativa de run, que retorna valores de scheme para primitivas y
valores de MiniScheme para clases y objetos
|#
(define (run-val s-expr)
  (define val (interp (parse s-expr) empty-env))
  (match val
    [(numV n) n]
    [(boolV b) b]
    [x x]))