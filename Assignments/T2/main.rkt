#lang play

#|
<expr> ::= <num>
         | <bool>
         | <id>
         | <string>
         | {if <expr> <expr> <expr>}
         | {fun {<id>*}}  <expr>}
         | {<expr> <expr>*}
         | {local {<def>*} <expr>}
         | {match <expr> <case>+}

<case> ::= {'case <pattern> '=> <expr>}
<pattern> ::= <num>
         | <bool>
         | <string>
         | <id>
         | (<constr-id> <attr-id>*)

<def>  ::= {define <id> <expr>}
         | {datatype <typename> <type-constructor>*}}


<type-constructor> ::= {<id> <member>*}
<constr-id> :: = <id>
<attr-id> :: = <id>
<typename> :: = <id>
<member>   :: = <id>

|#
; expresiones
(deftype Expr
  (num n)
  (bool b)
  (str s)
  (ifc c t f)
  (id s)
  (app fun-expr arg-expr-list)
  (prim-app name args)   ; aplicación de primitivas
  (fun id body)
  (lcal defs body)
  (mtch val cases))

; definiciones
(deftype Def
  (dfine name val-expr) ; define
  (datatype name variants)) ; datatype

; variantes
(deftype Variant
  (variant name params))

; estructuras de datos
(deftype Struct
  (structV name variant values))

; caso en pattern matching
(deftype Case
  (cse pattern body))

; patrón
(deftype Pattern
  (idP id) ; identificador
  (litP l) ; valor literal
  (constrP ctr patterns)) ; constructor y sub-patrones

;; parse :: s-expr -> Expr
(define(parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? boolean?) (bool s-expr)]
    [(? string?) (str s-expr)]
    [(? symbol?) (id s-expr)]
    [(list 'list s-exprs ...) (parse-list s-exprs)] ; Syntactic sugar
    [(list 'if c t f) (ifc (parse c) (parse t) (parse f))]
    [(list 'fun xs b) (fun xs (parse b))]
    [(list 'with (list (list x e) ...) b)
     (app (fun x (parse b)) (map parse e))]
    [(list 'local defs body)
     (lcal (map parse-def defs) (parse body))]
    [(list 'match val-expr cases ...) ; note the elipsis to match n elements
     (mtch (parse val-expr) (map parse-case cases))] ; cases is a list
    [(list f args ...) ; same here
     (if (assq f *primitives*)
         (prim-app f (map parse args)) ; args is a list
         (app (parse f) (map parse args)))]))

; parse-list :: s-expr -> List
(define (parse-list l)
  (match l
    [(list first rest ...)
     (app (id 'Cons) (list (parse first) (parse-list rest)))]
    [(list) (app (id 'Empty) (list))]))

; parse-def :: s-expr -> Def
(define(parse-def s-expr)
  (match s-expr
    [(list 'define id val-expr) (dfine id (parse val-expr))]
    [(list 'datatype name variants ...) (datatype name (map parse-variant variants))]))

; parse-variant :: sexpr -> Variant
(define(parse-variant v)
  (match v
    [(list name params ...) (variant name params)]))

; parse-case :: sexpr -> Case
(define(parse-case c)
  (match c
    [(list 'case pattern => body) (cse (parse-pattern pattern) (parse body))]))

; parse-pattern :: sexpr -> Pattern
(define(parse-pattern p)
  (match p
    [(? symbol?)  (idP p)]
    [(? number?)  (litP (num p))]
    [(? boolean?) (litP (bool p))]
    [(? string?)  (litP (str p))]
    [(list 'list elements ...) (parse-pattern-list elements)]
    [(list ctr patterns ...) (constrP (first p) (map parse-pattern patterns))]))

; parse-pattern-list : sexpr -> Pattern
; This function extends the parse-pattern in order to allow the list
; structure as a pattern.
(define (parse-pattern-list l)
  (match l
    [(list first rest ...)
     (constrP 'Cons (list (parse-pattern first) (parse-pattern-list rest)))]
    [(list) (constrP 'Empty (list))]))

; Values of the lenguage.
; We use cache to implement call-by-need 
(deftype Val
  (closureV arg body env)
  (exprV expr env cache))

;; interp :: Expr Env -> number/boolean/procedure/Struct
(define(interp expr env)
  (match expr
    ; literals
    [(num n) n]
    [(bool b) b]
    [(str s) s]
    ; conditional
    [(ifc c t f)
     (if (interp c env)
         (interp t env)
         (interp f env))]
    ; identifier
    [(id x) (env-lookup x env)]
    ; function (notice the meta interpretation)
    [(fun ids body)
     (closureV ids
               (λ (arg-vals) (interp body (extend-env ids arg-vals env)))
               env)]
    ; application
    [(app fun-expr arg-expr-list) (interp-app fun-expr arg-expr-list env)]
    ; primitive application
    [(prim-app prim arg-expr-list)
     (apply (cadr (assq prim *primitives*))
            (map (λ (a) (interp a env)) arg-expr-list))]
    ; local definitions
    [(lcal defs body)
     (def new-env (extend-env '() '() env))
     (for-each (λ (d) (interp-def d new-env)) defs)
     (strict (interp body new-env))]
    ; pattern matching
    [(mtch expr cases)
     (def value-matched (interp expr env))
     (def (cons alist body) (find-first-matching-case value-matched cases))
     (strict (interp body (extend-env (map car alist) (map cdr alist) env)))]))

; interp-app :: Expr List Env -> 
(define (interp-app fun-expr arg-expr-list env)
  (match (strict (interp fun-expr env))
    [(closureV arg body _)
     (body (map (λ (id a)
                     (match id
                       [(list 'lazy x) (exprV a env (box #f))]
                       [_ (strict (interp a env))]))
                   arg
                   arg-expr-list))]
    [inter (inter (map (λ (a) (interp a env)) arg-expr-list))]))

; interp-def :: Def Env -> Void
(define(interp-def d env)
  (match d
    [(dfine id val-expr)
     (update-env! id (interp val-expr env) env)]
    [(datatype name variants)
     ;; extend environment with new definitions corresponding to the datatype
     (interp-datatype name env)
     (for-each (λ (v) (interp-variant name v env)) variants)]))

; interp-datatype :: String Env -> Void
(define(interp-datatype name env)
  ; datatype predicate, eg. Nat?
  (update-env! (string->symbol (string-append (symbol->string name) "?"))
               (λ (v) (symbol=? (structV-name (first v)) name))
               env))

; interp-variant :: String String Env -> Void
(define(interp-variant name var env)
  ;; name of the variant or dataconstructor
  (def varname (variant-name var))
  ;; variant data constructor, eg. Zero, Succ
  (update-env! varname
               (λ (args) (structV name varname args))
               env)
  ;; variant predicate, eg. Zero?, Succ?
  (update-env! (string->symbol (string-append (symbol->string varname) "?"))
               (λ (v) (symbol=? (structV-variant (first v)) varname))
               env))

; strict : Val (exprV/closureV/numV/boolV) -> Val (closureV/numV/boolV)
(define (strict val)
  (match val
    [(exprV expr env cache)
     (if (unbox cache)
         (begin
           ; (printf "using cached value ~v~n" (unbox cache)) 
           (unbox cache))
         (let ([inval (strict (interp expr env))])
           ;(printf "Forcing exprV to ~v~n" inval)
           (set-box! cache inval)
           inval))]
    [_ val]))

;;;;; pattern matcher
(define(find-first-matching-case value cases)
  (match cases
    [(list) #f]
    [(cons (cse pattern body) cs)
     (let [(r (match-pattern-with-value pattern value))]
       (if (foldl (λ (x y)(and x y)) #t r)
           (cons r body)
           (find-first-matching-case value cs)))]))

(define(match-pattern-with-value pattern value)
  (match/values (values pattern value)
                [((idP i) v) (list (cons i v))]
                [((litP (bool v)) b)
                 (if (equal? v b) (list) (list #f))]
                [((litP (num v)) n)
                 (if (equal? v n) (list) (list #f))]
                [((constrP ctr patterns) (structV _ ctr-name str-values))
                 (if (symbol=? ctr ctr-name)
                     (apply append (map match-pattern-with-value
                                        patterns str-values))
                     (list #f))]
                [(x y) (error "Match failure")]))

; list data structure definition
(def list-struct '{datatype List
                            {Empty}
                            {Cons a b}})

; length :: List -> num
; function that receives a list and returns the length of it, but written
; in MiniScheme+ syntax.
(def length-function
  '{define length
     {fun {l}
          {match l
            {case {Cons a b} => {+ 1 {length b}}}
            {case {Empty} => 0}}}})


;; run :: s-expr -> number/boolean/procedura/struct
(define (run prog [flag ""])
  (begin
    (def given
      (interp (parse (list 'local (list list-struct length-function) prog))
                           empty-env))
    (match flag
      ["" given]
      ["pp" (pretty-printing-list given)]
      ["ppwu" (pretty-printing given)])))

#|-----------------------------
Environment abstract data type
empty-env   :: Env
env-lookup  :: Sym Env -> Val
extend-env  :: List[Sym] List[Val] Env -> Env
update-env! :: Sym Val Env -> Void
|#
(deftype Env
  (mtEnv)
  (aEnv bindings rest)) ; bindings is a list of pairs (id . val)

(def empty-env  (mtEnv))

(define(env-lookup id env)
  (match env
    [(mtEnv) (error 'env-lookup "no binding for identifier: ~a" id)]
    [(aEnv bindings rest)
     (def binding (assoc id bindings))
     (if binding
         (cdr binding)
         (env-lookup id rest))]))

(define (extend-env ids vals env)
  (aEnv (map cons ids vals) ; zip to get list of pairs (id . val)
        env))

;; imperative update of env, adding/overriding the binding for id.
(define(update-env! id val env)
  (set-aEnv-bindings! env (cons (cons id val) (aEnv-bindings env))))

;;;;;;;

;;; primitives
; http://pleiad.cl/teaching/primitivas
(define *primitives*
  `((+       ,(lambda args (apply + args)))
    (-       ,(lambda args (apply - args)))
    (*       ,(lambda args (apply * args)))
    (%       ,(lambda args (apply modulo args)))
    (odd?    ,(lambda args (apply odd? args)))
    (even?   ,(lambda args (apply even? args)))
    (/       ,(lambda args (apply / args)))
    (=       ,(lambda args (apply = args)))
    (<       ,(lambda args (apply < args)))
    (<=      ,(lambda args (apply <= args)))
    (>       ,(lambda args (apply > args)))
    (>=      ,(lambda args (apply >= args)))
    (zero?   ,(lambda args (apply zero? args)))
    (not     ,(lambda args (apply not args)))
    (and     ,(lambda args (apply (lambda (x y) (and x y)) args)))
    (or      ,(lambda args (apply (lambda (x y) (or x y)) args)))))

; to-str : number/boolean/id/string -> string
; returns an object as a string
(define (to-str obj)
  (match obj
    [(? number?)(string-append (number->string obj))]
    [(? symbol?)(string-append (symbol->string obj))]
    [(? boolean?)(string-append ((λ (b) (if b "#t" "#f")) obj))]
    [(? string?)(string-append obj)]))

; pretty-printing :: number/boolean/procedure/Struct -> string
; this function receives a data structure and returns the same structure
; but with a better visual representation.
(define (pretty-printing sV)
  (match sV
    ; structure with empty value
    [(structV _ name '())
     (string-append "{" (symbol->string name) "}")]
    ; Recursion :
    ; estructura con lista de valores
    [(structV _ name values)
     (string-append "{" (symbol->string name) " "
                    (foldr string-append "" (map pretty-printing values)) "}")]
    ; values within the structure (num, sym, bool, str)
    [id (string-append (to-str id))]
    ; empty list
    ['() ""]))

; pretty-printing-list :: number/boolean/procedure/Struct -> string
; this function extends the pretty-printing function in the list structure case.
(define (pretty-printing-list sV)
  (match sV
    ; list structure (extension)
    [(structV 'List _ values)
     (string-append "{list" (pretty-printing-list-vals values))]
    ; structure with empty value
    [(structV _ name '())
     (string-append "{" (symbol->string name) "}")]
    ; Recursion :
    ; estructura con lista de valores
    [(structV _ name values)
     (string-append "{" (symbol->string name) " "
                    (foldr string-append "" (map pretty-printing values)) "}")]
    ; values within the structure (num, sym, bool, str)
    [id (to-str id)]
    ; empty list
    ['() ""]))

; pretty-printing-list-vals : number/boolean/procedure/Struct -> string
; This function covers the list 
(define (pretty-printing-list-vals l)
  (match l
    [(list value (structV 'List _ values))
     (string-append " "(pretty-printing-list value) (pretty-printing-list-vals values))]
    [(list) "}"]))