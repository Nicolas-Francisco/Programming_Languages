#lang play

; ------------------------------------------------- P1:
#| <formula> ::=
| (Bool <Bool>)
| (id <sym>)
| (^ <formula> <formula>)
| (v <formula> <formula>)
| (with <sym> <formula> <formula>)
|#

(deftype Formula
  [bool b]
  [id x]
  [^ left right]
  [v left right]
  [with x expr body])

(deftype FValue
  [BoolV bool])

; parser
(define (parse s-expr)
  (match s-expr
    ['True (bool #t)]
    ['False (bool #f)]
    [(? symbol?) (id s-expr)]
    [(list '^ l r) (^ (parse l) (parse r))]
    [(list 'v l r) (v (parse l) (parse r))]
    [(list 'with (list x e) b)
     (with x (parse e) (parse b))]))


#| -----------------------------
Environment abstract data type

empty-env :: Env
extend-env :: Sym LValue Env -> Env
env-lookup :: Sym Env -> FValue

representation BNF:

<env> ::= (mtEnv)
| (aEnv <id> <FValue> <env>)

|#
(deftype Env
  (mtEnv)
  (aEnv id val env))

(define empty-env  (mtEnv))
(define extend-env aEnv)
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest)
     (if (eq? id x)
         val
         (env-lookup x rest))]))

; ; interp :: Formula Env -> FValue
(define (interp expr env)
  (match expr
    [(bool val) (if val (BoolV val) (BoolV val))]
    [(id x) (env-lookup x env)]
    [(^ l r) (and (interp r env) (interp l env))]
    [(v l r) (or (interp r env) (interp l env))]
    [(with x e b)
     (interp b (extend-env x (interp e env) env))]))

; run : Src -> Val
(define run
  (compose interp parse))


; ------------------------------------------------- P2:

; filterTR ::
; function filter but with tail recursion
(define (filterTR prod list)
  (if (eq? '() list)
      '()
      (if (prod (first list))
          (cons (first list) (filterTR prod (rest list)))
          (filterTR prod (rest list)))))