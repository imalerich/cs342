#lang racket
(provide (all-defined-out))

;; This is for eval
(define prog2
  '(var ((x z) (y (+ x 1)))
	(+ x y)
))

;; Sample Run: Returns 21
;; (eval prog2 '((z 10)))

;; Sample Run: Returns '(Cannot Evaluate)
;; (eval prog2 '()))

;;;;;;;;;;;;;;;;;;
;;	CCond	;;
;;;;;;;;;;;;;;;;;;

;; All tests of the form bcondX
;; should also pass the ccond? test.
;; As an example (ccond? bcond0) -> True.

;; True
(define ccond0 '(or (gt 3 3) (lt 4 5)))

;; True
(define ccond1 (list 'and ccond0 '(lt 4 5)))

;; True
(define ccond2 (list 'not ccond1))

;; True
(define ccond3 (list 'and ccond2 (list 'or ccond2 (list 'not ccond0))))

;; False
(define ccond4 (list 'and '3 '4))

;; False (only takes one ccond argument)
(define ccond5 (list 'not ccond1 ccond2))

;; False (requires two ccond arguments)
(define ccond6 (list 'and '(lt 3 4)))

;; False (ccond0 is a constant, not a reference)
(define ccond7 '(and ccond0))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	BinaryCond	;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; True
(define bcond0 '(gt 3 3))

;; True
(define bcond1 '(lt 4 5))

;; True
(define bcond2 '(eq (x 3) 3))

;; False
(define bcond3 '(gt 3 4 3))

;; False
(define bcond4 '(greaterthan 3 4 3))

;; False
(define bcond5 '(gteq 3 4 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	Variables	;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; True
(define var0 'x)

;; False
(define var1 '(x))

;; False
(define var2 '(3))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	Operators	;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; True
(define op0 '+)

;; True
(define op1 '-)

;; True
(define op2 '*)

;; True
(define op3 '/)

;; False
(define op4 '(*))

;; False
(define op5 '(/))

;; False
(define op6 'x)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	VarAssign	;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; True
(define varassign0
  '((x 3) (y 2) (z 6))
)

;; True
(define varassign1
  '((x 3))
)

;; True
(define varassign2
  '()
)

;; False
(define varassign3
  '((x 3 2))
)

;; False
(define varassign4
  '((x 3) 4)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	CCondExpr	;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; True
(define ccondexpr0
    (list ccond0 3 4)
)

;; True
(define ccondexpr1
    (list ccond1 3 4)
)

;; True
(define ccondexpr2
    (list ccond2 3 4)
)

;; True
(define ccondexpr3
    (list ccond3 3 4)
)

;; True
(define ccondexpr4
    (list bcond0 3 4)
)

;; True
(define ccondexpr5
    (list bcond1 3 4)
)

;; False (need one less expr)
(define ccondexpr6
    (list ccond3 3 4 6)
)

;; False (need one more expr)
(define ccondexpr7
    (list ccond3 3)
)

;; False (ccond6 is not a CCond)
(define ccondexpr8
    (list ccond6 3 4)
)
