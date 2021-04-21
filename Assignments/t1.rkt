#lang play

; <Prog>   ::= {<Fundef>* <Expr>}
; Program structure
(deftype Prog
  (f-defs fundef expr))

; <FunDef> ::= {define {<id> <id>*} <Expr>}
; Function definition structure
(deftype FunDef
  (emFunDef)
  (d-fundef id rest-id expr))

#|
<expr>   ::= <num>
           | <id>
           | <bool>           
           | {<unop> <expr>}
           | {<binop> <expr> <expr>}
           | {if <expr> <expr> <expr>}
           | {with {{<id> <expr>}*} <expr>}
           | {<id> <expr>*}
|#
(deftype Expr
  (null)
  (num n)
  (id x)
  (bool b)
  (un un-op expr)
  (bin bi-op l r)
  (if con t-branch f-branch)
  (with id n-expr body)
  (aExpr id expr))

(deftype Env
  (mtEnv)
  (aEnv id val env))

; <unop>   ::= ! | add1 | sub1  
(deftype Unop)

; <binop>  ::= + | - | * | / | && | = | < | ...
(deftype Binop)