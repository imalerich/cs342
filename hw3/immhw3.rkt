#lang racket
(provide (all-defined-out))

;; This will be returned when evaluation fails.
(define ERR '(Cannot Evaluate))

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

(define (eval prog env)
    (if (env.valid? env)	;; If we have a vaild environment
	((eval.expr prog) env)	;; go ahead and do evaluation
	ERR			;; else display an error.
))

;;;;;;;;;;;;;;;;;
;; Environemnt ;;
;;;;;;;;;;;;;;;;;

;; Checks the syntax of the input environment.
;; Returns TRUE if the environment has correct syntax, FALSE otherwise.
;; A valid environment is defined simply as a list of ordered pairs.
;; \param env The environment we wish to evaluate.
(define env.valid?
    (lambda (env)
	(if (list? env)
	    (if (null? env)
		#T ;; An empty list is a valid environment.
		(and 
		    (list? (car env))			;; First element must be a list
		    (equal? (length (car env)) 2)	;; containing two elements.
		    (number? (cadr (car env)))		;; Second must be a number (eager evaluation).
		    (env.valid? (cdr env))		;; The remaining elemnts must also be valid.
	    ))
	    #F	;; An environment must be a list by definition.
)))

;; Checks whether or not the environment contains the given variable,
;; the variable is treated as a key into the set of ordered pairs in the environment.
;; Returns TRUE if the var is found as a key in the environment, FALSE otherwise.
;; \param env The environment to search through.
;; \param var The kep we are looking for in env.
(define env.contains?
    (lambda (env)
	(lambda (var)
	    (if (list? env)
		(if (null? env)
		    #F						;; Base case, variable not found.
		    (if (equal? (car (car env)) var)
			#T					;; Found the variable,
			((env.contains? (cdr env)) var)	;; else keep recursing.
		))
		#F						;; Dude we at least need a list.
))))

;;;;;;;;;;;;;;;;
;; Evaluation ;;
;;;;;;;;;;;;;;;;

(define eval.expr
    (lambda (expr)
	(lambda (env)
	    (cond 
		[(number? expr) (eval.number expr)]
		[(variable? expr) ((eval.variable expr) env)]
		[(opexpr? expr) ((eval.opexpr expr) env)]
		[else ERR]
))))

;; No environemnt is needed, a number is just the expression itself
;; \param number
(define eval.number
    (lambda (expr)
	expr		;; The symmantics is simply the given number.
))

;; \param expr	An expression that has been verified to be a variable.
;; \param env	The environment in which the given variable should reside.
;; 		If the variable is not present, an error is returned.
;;		This assume the input environment is a list of ordered pairs (or empty).
(define eval.variable
    (lambda (expr)
	(lambda (env)
	    (if (null? env)
		ERR	;; Could not find the variable in the environment.
		(if (equal? (car (car env)) expr)
		    (cadr (car env))			;; Found what we are looking for,
		    ((eval.variable expr) (cdr env))	;; else recurse to keep looking.
		)
))))

(define eval.opexpr
    (lambda (expr)
	(lambda (env)
	    (cond
		[(arithexpr? expr) ((eval.arithexpr expr) env)]
		[(condexpr? expr) ((eval.condexpr expr) env)]
		[(varexpr? expr) ((eval.varexpr expr) env)]
		[else ERR]
))))

;; Assumes the input expression has been verified as an arithexpr.
;; If so we can make assumptions about the construction of expr.
;; \param expr	This should be a valid arithmetic expression list.
;; \param env	The environment in which we are evaluating.
(define eval.arithexpr
    (lambda (expr)
	(lambda (env)
	    (if (and ;; Evaluating both expressions should result in a number.
		    (number? (eval (cadr expr) env))
		    (number? (eval (caddr expr) env)))
		(cond ;; We know we have numbers to work with, go ahead and do some arithmetic.
		    [(equal? (car expr) '+) (+ (eval (cadr expr) env) (eval (caddr expr) env) )]
		    [(equal? (car expr) '-) (- (eval (cadr expr) env) (eval (caddr expr) env) )]
		    [(equal? (car expr) '*) (* (eval (cadr expr) env) (eval (caddr expr) env) )]
		    [(equal? (car expr) '/) (/ (eval (cadr expr) env) (eval (caddr expr) env) )]
		    [else ERR] ;; This should never happen, cause we know our arithexpr is valid.
		)
		ERR ;; Expressions do not result in numbers, cannot evaluate.
))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditional Evaluation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eval.condexpr
    (lambda (expr)
	(lambda (env)
	    ;; Evaluate the given conditional in our environment.
	    (if ((eval.ccond (car expr)) env)
		;; If TRUE, evaluate the first expression,
		(eval (cadr expr) env)
		;; else evaluate the second.
		(eval (caddr expr) env)
))))

;; Evaluates a conditional expression to TRUE or FALSE.
(define eval.ccond
    (lambda (expr)
	(lambda (env)
	    (cond
		[(equal? (car expr) 'or)
		    (or
			((eval.ccond (cadr expr)) env)
			((eval.ccond (caddr expr)) env)
		)]
		[(equal? (car expr) 'and)
		    (and
			((eval.ccond (cadr expr)) env)
			((eval.ccond (caddr expr)) env)
		)]
		[(equal? (car expr) 'or)
		    (not
			((eval.ccond (cadr expr)) env)
		)]
		[else ((eval.bcond expr) env)]
))))

;; Takes an input binary conditional expression (gt, lt, eq), evaluates
;; both parameters in the input environment, then returns teh result of the
;; conditional on the two results.
;; \param expr	Binary expression to evaluate.
;; \param env	Environment in which to evaluate parameters.
;; \return	True if the conditional evaluates to True, False otherwise.
(define eval.bcond
    (lambda (expr)
	(lambda (env)
	    (cond
		[(equal? (car expr) 'gt) 
		    (>
			(eval (cadr expr) env)
			(eval (caddr expr) env)
		)]
		[(equal? (car expr) 'lt) 
		    (<
			(eval (cadr expr) env)
			(eval (caddr expr) env)
		)]
		[(equal? (car expr) 'eq) 
		    (equal?
			(eval (cadr expr) env)
			(eval (caddr expr) env)
		)]
		[else ERR]
))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable Evaluation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Given an expression of the form (var VarAssign Expr), creates a new environment
;; by appending the results of VarAssign, this new environment is then
;; used to evaluate Expr, whose result will be returned.
(define eval.varexpr
    (lambda (expr)
	(lambda (env)
	    ;; Evaluate the expression in our new environment.
	    (eval (caddr expr) ((eval.varassign (cadr expr)) env))
)))

;; Given a list of variable assignments (as a list of orderd pairs), 
;; return a new environment created with new variable assignments appended.
;; If a variable is given by an expression, that expression will be evaluated
;; and its numerical value will be stored in the environment (eager evaluation).
;; \param expr	List of variable assignment sequences.
;; \param env	Used to evaluate varassign expressions, new variables
;;		will be appended to this environment in the return value.
;; \return	Environment after variable assignments have been completed.
(define eval.varassign
    (lambda (expr)
	(lambda (env)
	    (if (null? expr)
		env	;; Base case - done constructing the environment.
		((eval.varassign (cdr expr)) ;; Evaluate the remaining variables
		    (cons ((eval.varassignment (car expr)) env) env)) ;; in our new environment.
))))

;; Takes a single variable assignment as an orderd pair
;; in the form of (Variable Expr) and evaluates 'Expr'
;; returns the orderd pair (Variable (eval Expr))
;; to be stored in the environment (eager evaluation).
(define eval.varassignment
    (lambda (expr)
	(lambda (env)
	    (if (number? (eval (cadr expr) env)) ;; Can only assign a number to a variable.
		(list (car expr) (eval (cadr expr) env))	;; Evaluate the expression,
		ERR						;; else return an error.
))))

;;;;;;;;;;;;;;;;;;;;;
;; Syntax Checking ;;
;;;;;;;;;;;;;;;;;;;;;

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
