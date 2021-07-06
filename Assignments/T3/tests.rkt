#lang play
(require "main.rkt")
(print-only-errors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                 TESTS BASE                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (run-val '(+ 1 2)) 3)
(test (run-val '(< 1 2)) #t)
(test (run-val '(- 2 1)) 1)
(test (run-val '(* 2 3)) 6)
(test (run-val '(= (+ 2 1) (- 4 1))) #t)
(test (run-val '(and #t #f)) #f)
(test (run-val '(or #t #f)) #t)
(test (run-val '(not (not #t))) #t)
(test (run-val '(if (not #f) (+ 2 1) 4)) 3)
(test (run-val '(local ([define x 5])
              (seqn {+ x 1}
                    x))) 5)

;; Ejemplos del enunciado
(test (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method sum (z) (+ (get x) (+ (get y) z)))
                          (method set-x (val) (set x val))
                          (method get-y () (get y))))]
            (seqn
             (send o set-x (+ 1 3))
             (+ (send o sum 3) (send o get-y)))))
      11)

(test (run-val
       '(local
            [(define a
               (object
                (method auto-apply (o)
                        (send o apply o))
                (method foo () 5)
                ))
             (define o (send a auto-apply
                             (object
                              (method apply (other) (send other apply2 this))
                              (method apply2 (other) this)
                              (method foo () 42))))]
          (send o foo)))
      42)

(test (run-val '(local
              [(define smart-computer (object
                                       (method secret? (something) 42)))
               (define everything (object))
               (define oracle (object : smart-computer))]
               (send oracle secret? everything)))
      42)

(test (run-val '(local
              [(define seller (object
                               (method multiplier () 1)
                               (method price (item-number)
                                       (* item-number (send this multiplier)))))
               (define broker (object : seller
                                      (method multiplier () 2)))]
               (send broker price 3)))
      6)

(test (run-val '(local
                    ([define x (object
                                (field z 3)
                                (method get () (get z)))]
                     [define y (object : x)])
                  (send y get)))
      3)

(test/exn (run-val '(local
                        ([define x (object
                                    (field z 3)
                                    (method get () (get z)))]
                         [define y (object
                                    : x
                                    (method get () (get z)))])
                      (send y get)))
          "field not found")

;; A simple monotone counter
(define counter '(object
                  (field count 0)
                  (method incr () (set count (+ 1 (get count))))
                  (method get () (get count))))

(define (incrs-multiply x y)
  `(seqn
    (send ,y incr)
    (seqn
     (send ,x incr)
     (seqn
      (send ,x incr)
      (* (send ,x get) (send ,y get))
      ))))

(test (run-val
       `(local ([define c ,counter])
          (seqn (send c incr)
                (local ([define c2 (shallow-copy c)])
                  ,(incrs-multiply 'c 'c2)))))
      6)

(test (run-val
       `(local ([define c (object : ,counter)])
          (seqn (send c incr)
                (local ([define c2 (shallow-copy c)])
                  ,(incrs-multiply 'c 'c2)))))
      16)

(test (run-val
       `(local ([define c (object : ,counter)])
          (seqn (send c incr)
                (local ([define c2 (deep-copy c)])
                  ,(incrs-multiply 'c 'c2)))))
      6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                  SUS TESTS                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-only basic test
(test (run-val '(local
              [(define o (object
                          (field y 2)
                          (method get-y () (get y))))]
            (send o get-y))) 2)

; get basic test
(test (run-val '(send (object (field y 2) (method get-y () (get y))) get-y))
      2)

; get error test
(test/exn (run-val '(+ 2 (get x)))
          "get used outside of an object")

; set basic test
(test (run-val '(local
              [(define o (object
                          (field y 2)
                          (method set-y (val) (set y val))
                          (method get-y () (get y))))]
            (seqn
             (send o set-y 3)
             (send o get-y))))
      3)

; get error test
(test/exn (run-val '(set x 2))
          "set used outside of an object")

; "unavailable word" test
(test/exn (run-val '(object (field this 3)))
          "unavailable word")

; this basic test
(test (run-val '(local
                  [(define o (object
                              (method foo () (send this bar))
                              (method bar() 22)))]
                  (send o foo)))
      22)
; capture outside identifiers
(test (run-val '(local [(define x 2)
                        (define o (object
                                   (field y x)
                                   (method get-y () (get y))))]
                  (send o get-y)))
      2)

; outside identifiers can be the same as the inside fields
(test (run-val '(local [(define x 2)
                         (define o (object
                                    (field x x)
                                    (method get-x () (get x))))]
                        (send o get-x)))
      2)

; can't edit outside values
(test (run-val '(local [(define x 2)
                         (define o (object
                                    (field y x)
                                    (method set-y (val) (set y val))))]
                        (seqn
                         (send o set-y 3)
                         x)))
      2)

; get can be used inside of a set method
(test (run-val '(local [(define o (object
                               (field x 2)
                               (method get-x () (get x))
                               (method set-x (val) (set x (+ val (get x))))))]
           
            (seqn
             (send o set-x 3)
             (send o get-x))))
      5)

; shallow-copy basic test
(test (run-val '(local [(define o (object
                               (field x 2)
                               (method get-x () (get x))
                               (method set-x (val) (set x (+ val (get x))))))
                    (define o1 (shallow-copy o))]
            (seqn
             (send o set-x 3)
             (send o1 get-x))))
      2)

; can't find fields outside object
(test/exn (run-val '(local
               ([define x (object
                           (field z 3)
                           (method get () (get u)))]
                [define y (object : x
			  (field u 4))])
             (send y get)))
          "field not found")

; bonus
(test (run-val '(local
              [(define f (fun (x)
                              (+ x x)))]
              (f 5)))
10)

; λ function with 2 argumments
(test (run-val '(local
              [(define f (fun (x y)
                              (+ x y)))]
              (f 5 20)))
25)