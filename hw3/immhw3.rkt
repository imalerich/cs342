#lang racket
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;
;;	1	;;
;;;;;;;;;;;;;;;;;;

(define synchk
    (lambda (prog)
	(expr? prog)
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
;;	Syntax Checking	;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (expr? e)
    (or
	(number? e)	;; Either a Number
	(variable? e)	;; or a Variable
	(opexpr? e)	;; or an OpExpr.
))

(define (opexpr? e)
    (or
	(arithexpr? e)	;; Either an ArithExpr
	(condexpr? e)	;; or CondExpr
	(varexpr? e)	;; or VarExpr.
))

(define (arithexpr? e)
    (and
	(list? e)		;; Must be a list
	(equal? (length e) 3)	;; of length 3.
	(op? (car e))		;; First element is an operator,
	(expr? (cadr e))	;; followed by the first operand
	(expr? (caddr e))	;; followed by the second operand.
))

(define (op? o)
    (or 
	(equal? o '+)
	(equal? o '-)
	(equal? o '*)
	(equal? o '/)
))

(define (condexpr? c)
    (and
	(list? c)		;; Must be a list
	(equal? (length c) 3)	;; of length 3.
	(ccond? (car c))	;; First element is a CCond,
	(expr? (cadr c))	;; followed by an expression evaluated on true
	(expr? (caddr c))	;; and an expression evaluated on false.
))

(define (ccond? c)
    (if (list? c)
	(cond
	    [(bcond? c) #T]			;; Might be a BCond...
	    [(and 
	       (equal? (length c) 3)		;; Otherwise, if it is a list of length 3
	       (or (equal? (car c) 'or)		;; could be an 'OR' expression,
		    (equal? (car c) 'and))	;; or an 'AND' expression,
	       (ccond? (cadr c))		;; both of which have a first argument of type CCond
	       (ccond? (caddr c))		;; and a second argument of type ccond.
	    ) #T]				
	    [(and
		(equal? (length c) 2)		;; Else if it is a list of length 2
		(equal? (car c) 'not)		;; it must be a NOT expression
		(ccond? (cadr c))		;; followed ba a CCond
	    ) #T]				
	    [else #F]				;; Conditions not satisfied, not a CCond.
	)

	#F					;; A CCond must be a list.
))

(define (bcond? b)
    (if (list? b)
	(if (equal? (length b) 3)
	    (and 
		(bcondop? (car b))	;; First element should be a BCond Operator (gt, lt, eq),
		(expr? (cadr b))		;; the second should be an expression,
		(expr? (caddr b)))	;; as should the third,
	    #F				;; and we shouldn't have any elements after that.
	    )
	#F				;; A BCond must be a list.
))

;; Checks if the input item is 
;; any of the constants 'gt (greater than)', 'lt (less than)', 
;; or 'eq (equal to)'.
(define (bcondop? o)
    (or 
	(equal? o 'gt)
	(equal? o 'lt)
	(equal? o 'eq)
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
