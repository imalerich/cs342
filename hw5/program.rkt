#lang racket
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;
;; Simple Tests ;;
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Old Function Samples ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (eval prov2 ’((x 3)) '()) returns '(6 ())
;; (eval prov2 ’((x 4)) '()) returns '(24 ())
(define prov2
    '(fun ((f1 (x)) ((gt x 0)
	(* x (apply (f1 ((- x 1)))))
	1))
    (apply (f1 (x))
)))

;; (eval prov3 '((y 10)) '()) = '(12 ())
(define prov3
  '(fun ((f (a b)) (var ((x a) (y b)) (+ x y))) (apply (f (y 2))
)))

;; (eval prov4 '() '()) = '(2 ())
(define prov4
    '(var ((x 1))
	(fun ((f (x)) x)
	    (fun ((g ()) (var ((x (+ x 1))) (apply (f (x)))))
		(apply (g ()))
))))

;; (eval prov5 '() '()) = '(1 ())
(define prov5
    '(var ((x 1))
	(fun ((f ()) x)
	    (fun ((g ()) (var ((x (+ x 1))) (apply (f ()))))
		(apply (g ()))
))))

;; (eval prov6 '((x 10)) '()) = '(55 ())
(define prov6
    '(fun ((f (n))
	((eq n 0) 0 ((eq n 1) 1 (+ (apply (f ((- n 1)))) (apply (f ((- n 2))))))))
	    (apply (f (x)))
))

;; (eval prov7 '((x 10)) '()) = '(3628800 ())
(define prov7
    '(fun ((f (n a))
	((eq n 0) a (apply (f ((- n 1) (* n a))))))
	    (fun ((g (n)) (apply (f (n 1))))
		(apply (g (x)))
)))

;; VALID
;; Returns pi.
;; Results in 4.14159265
(define prog0
    '(fun ((pi ()) 3.14159265) 
	(apply (pi ())
)))

;; VALID
;; Adds one to pi.
;; Results in 4.14159265
(define prog1
    '(fun ((pi ()) 3.14159265) 
	(+ 1 (apply (pi ()))
)))

;;;;;;;;;;;;;;
;; FAILS!!! ;;
;;;;;;;;;;;;;;

;; VALID
;; Adds two, then adds one to z,
;; where z must be provided by the input environment.
;; (eval prog3 '((z 2))) returns 5
(define prog3
    '(fun ((addone (x)) (+ x 1))
	(fun ((addtwo (y)) (+ y 2))
	    (apply (addone (
		(apply (addtwo (z)))
))))))

;; VALID
;; Program -> Expr -> Number
(define prog5 525600)

;; VALID
;; Program -> Expr -> Variable
(define prog6 'x)

;; VALID
;; same as prog8, but pi is defined
;; evaluates to 0, as !(pi > 1) evaluates to FALSE
(define prog9
    '(fun ((pi ()) 3.14159265) 
	((not (gt (apply (pi ())) 1))
	    1
	    0
)))

;; VALID
;; Calls function pi to assign pi to x,
;; then returns 2 * PI = 6.2831853
(define prog12
    '(fun ((pi ()) 3.14159265) 
	(var
	    ((x (apply (pi ()))))
	    (* 2 x)
)))
