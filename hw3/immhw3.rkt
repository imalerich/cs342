#lang racket
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;
;;	1	;;
;;;;;;;;;;;;;;;;;;

(define synchk
    (lambda (prog)
      #T
))

;;;;;;;;;;;;;;;;;;
;;	2	;;
;;;;;;;;;;;;;;;;;;

(define eval
    (lambda (prog)
	(lambda (env)
	    #T
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	Utilities	;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (expr? e)
    #T
)

(define (op? o)
    (or (equal? o '+)
	(equal? o '-)
	(equal? o '*)
	(equal? o '/)
))

(define (varexpr? v)
    (if (list? v)
	(if (equal? (length v) 3)
	    (and (and
		(equal? (car v) 'var)	;; A variable expression is of the form (var
		(varassign? (cadr v))	;; followed by a VarAssign
		(expr? (caddr v))	;; then Expr)
	    ))
	    #F 				;; with nothing else in it.
	)
	#F				;; It certainly at least has to be a list.
))

(define (varassign? v)
    (if (list? v)
	(if (null? v)
	    #T				;; Base case of recursion.
	    (and 
		(variableexpr? (car v))	;; If the front element is of the form (Variable Expr)
		(varassign? (cdr v)))	;; and all tail elements are also of that form.
	)
	#F
))

;; Is the input list of the form (Variable Expr)
(define (variableexpr? v)
    (if (list? v)
	(cond 
	    [(and (and 
		(equal? (length v) 2)	;; A list of length two
		(variable? (car v)))	;; with a variable
		(expr? (cadr v))) 	;; followed by a variable expression
		    #T]			;; is a valid VarAssignSeq.
	    [else #F]
	)
	#F				;; Not a list? Not a Variable Assignment.
))

(define (variable? v)
    (symbol? v)
)
