#lang racket
(provide (all-defined-out))

(require "program.rkt")

(define ERR '(Cannot Evaluate))
(define FMA '(exception fma))
(define OOMA '(exception ooma))
(define OOM '(exception oom))

;; Evaluates the input program in the given environment.
;; \param prog	Program which we wish to evaluate.
;; \param env	The environment in which to run the given program.
;; \param heap	The heap the expression should use for evaluation.
;; \return	The evaluation of the given program if it can be 
;;		evaluated, else returnns '(Cannot Evaluate).
;;		The syntax of the program must be correct in order
;;		for the program to be evaluated, further all free
;;		variables in the program must be provided.
(define (eval prog env heap)
    (if (env.valid? env)		;; If we have a vaild environment
	(eval.expr prog env heap)	;; go ahead and do evaluation,
	ERR				;; else display an error.
))

;;;;;;;;;;;;;;;;
;; Evaluation ;;
;;;;;;;;;;;;;;;;

;; Root abstraction for program evaluation.
;; Determines which type of evaluation we need to start with
;; and then start the recursing through the expression.
;; If the expression cannot be evaluated, ERR is returned.
;; \param expr	The expression with which to evaluate.
;; \param env	The environment the expression is to be evaluated in.
;; \param heap	The heap the expression should use for evaluation.
;; \return	The results of the expression, ERR if an error occurs.
(define (eval.expr expr env heap)
    (cond 
	[(number? expr) (list (eval.number expr) heap)]
	[(variable? expr) (list ((eval.variable expr) env) heap)]
	[(opexpr? expr) (eval.opexpr expr env heap)]
	[(fexpr? expr) (eval.fexpr expr env heap)]
	[(applyf? expr) (eval.applyf expr env heap)]
	[(dref? expr) (eval.dref expr env heap)]
	[(wref? expr) (eval.wref expr env heap)]
	[(ref? expr) (eval.ref expr env heap)]
	[(free? expr) (eval.free expr env heap)]
	[else expr]
))

;;;;;;;;;;;;;;;;
;; References ;;
;;;;;;;;;;;;;;;;

;; Evaluates the expression, searches the heap for using the evaluation result as key
;; then returns a (result heap) pair of the heap post expression evaluation and the
;; result found in the heap.
(define (eval.dref expr env heap)
    (if (equal? 
	    (heap.find heap (car (eval.expr (cadr expr) env heap)))
	    OOMA)
	OOMA ;; ERROR - Could not find requested location.
	(if (equal? ;; Make sure we are not trying to read from a 'free' location.
	    (heap.find heap (car (eval.expr (cadr expr) env heap))) 'free)
	    FMA ;; ERROR - Requested location is 'free'.
	    (list
		;; Result is the value in the input heap.
		(heap.find heap ;; Search the input heap.
		    (car (eval.expr (cadr expr) env heap))) ;; Evaluate the expression to find the key.
		(cadr (eval.expr (cadr expr) env heap))) ;; The input expr may have modified the heap.
)))

;; Evaluates the expression for the desired key and value, then modifies the heap by changing
;; the value at key with the evaluated value. Both the key and value evaluation may also perform
;; changes on the heap. Pretty sure the assignment does not require this, but I have included it just in case.
(define (eval.wref expr env heap)
    (if (heap.key.exists? heap (car (eval.expr (cadr expr) env heap)))
	;; We have evaluated a valid key, now write the value to the heap.
	(list
	    ;; The value is simply the one passed in by Expr2.
	    (car (eval.expr (cadr expr) env 
		(cadr (eval.expr (cadr expr) env heap)) ;; Let Expr1 modify the heap.
	    ))
	    ;; Modify the heap here...
	    (heap.write ;; This will return the modified heap.
		;; Include modifications by both key and value evaluation.
		(cadr (eval.expr (cadr expr) env 
		    (cadr (eval.expr (cadr expr) env heap)) ;; Let Expr1 modify the heap.
		))
		;; Key goes here.
		(car (eval.expr (cadr expr) env heap))
		;; And Value goes here.
		(car (eval.expr (cadr expr) env 
		    (cadr (eval.expr (cadr expr) env heap)) ;; Let Expr1 modify the heap.
		))
	))
	FMA ;; ERROR - Key does not exist for writing.
))

;; Evaluates the input expression to a value, then writes that value to the first available free location.
;; If no such free location is found, then OOM is returned instead.
(define (eval.ref expr env heap)
    (if (equal? (heap.first.free heap) OOM)
	OOM ;; ERROR - We have no where to write this data.
	;; Else we know we have a valid key, so write our data to that key.
	(heap.write
	    heap
	    (heap.first.free heap) ;; The key we are writing to.
	    (car (eval.expr (cadr expr) env heap)) ;; The value to be written.
)))

;; Evaluates the expression for the given key, if that key exists (and is not already free), 
;; then 'free' will be written to that location in memory and the (key heap) will be returned.
;; If either the location is already free or the key does not exist, the FMA exception is returned.
(define (eval.free expr env heap)
    (if (heap.key.exists? heap (car (eval.expr (cadr expr) env heap)))
	(if (equal? (heap.find heap (car (eval.expr (cadr expr) env heap))) 'free)
	    FMA ;; ERROR - Value at key has already been freed.
	    (list
		;; Returns the evaluated key.
		(car (eval.expr (cadr expr) env heap))
		;; Accompanied by the modified heap with freed memory.
		(heap.write
		    heap
		    (car (eval.expr (cadr expr) env heap))
		    'free
	    )))
	FMA ;; ERROR - Key does not exist for deallocation.
))

;; Finds the first key in the heap that has the value 'free'.
;; If no such location exists, the OOM exception is returned instead.
(define (heap.first.free heap)
    (if (or (null? heap) (not (list? heap)))
	OOM
	(if (equal? (cadr (car heap)) 'free)
	    (car (car heap)) ;; A free loctaion was found, return the key.
	    (heap.first.free (cdr heap)) ;; Recurse to the tail of the heap.
)))

;; Searches a list of key value pairs for the given key.
(define (heap.find heap key)
    (if (or (null? heap) (not (list? heap)))
	OOMA ;; We could not find the address to dereference.
	(if (equal? key (car (car heap)))	;; If the key matches the front item in the heap...
	    (cadr (car heap))			;; return the value of the front item,
	    (heap.find (cdr heap) key)		;; else recurse to the rest of the heap.
)))

;; Determines whether or not the key is actually present in the given heap.
(define (heap.key.exists? heap key)
    (if (or (null? heap) (not (list? heap)))
	#F
	(if (equal? key (car (car heap)))
	  #T
	  (heap.key.exists? (cdr heap) key)
)))

;; Writes 'val' to the location given by 'key' in the input heap.
;; Assumes key is valid, if it is not, this function does nothing.
;; Returns the modified heap.
(define (heap.write heap key val)
    (if (or (null? heap) (not (list? heap)))
	heap ;; Base case - return the unmodified heap.
	(if (equal? key (car (car heap)))
	    (cons
		(list key val) ;; Replace the front element with the given key value pair.
		(cdr heap)) ;; No need to change the rest of the heap.
	    (cons
		(car heap) ;; Keep the front of the heap the same...
		(heap.write (cdr heap) key val)) ;; but modify the rest.
)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Numbers & Operators ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; No environemnt is needed, a number is just the expression itself
;; \param number
(define eval.number
    (lambda (expr)
	expr	;; The symmantics is simply the given number.
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
)))))

;; Redirect the expression to the appropriate operator handler.
;; If the input expression is not an ArithExpr, CondExpr, or VarExpr
;; an error will be returned.
;; \param expr	Operator expression to evaluate.
;; \param env	The environment in which to evaluate the expression.
(define (eval.opexpr expr env heap)
    (cond
	[(arithexpr? expr) (eval.arithexpr expr env heap)]
	[(condexpr? expr) (eval.condexpr expr env heap)]
	[(varexpr? expr) (eval.varexpr expr env heap)]
	[else ERR]
))

;; Assumes the input expression has been verified as an arithexpr.
;; If so we can make assumptions about the construction of expr.
;; \param expr	This should be a valid arithmetic expression list.
;; \param env	The environment in which we are evaluating.
(define (eval.arithexpr expr env heap)
    (if (and ;; Evaluating both expressions should result in a number.
	    (number? (car (eval.expr (cadr expr) env heap)))
	    (number? (car (eval.expr (caddr expr) env 
		(cadr (eval.expr (cadr expr) env heap)) ;; Use the heap from the evalution of the first operand.
	    ))))
	(cond ;; We know we have numbers to work with, go ahead and do some arithmetic.
	    [(equal? (car expr) '+) (list (+ 
		(car (eval.expr (cadr expr) env heap)) ;; Firt expression using the initial heap...
		(car (eval.expr (caddr expr) env 
		    (cadr (eval.expr (cadr expr) env heap)) ;; Second expression using modified heap...
		)))
		(cadr (eval.expr (caddr expr) env 
		    (cadr (eval.expr (cadr expr) env heap)) ;; Include final heap in the result.
		))
	    )]
	    [(equal? (car expr) '-) (list (- 
		(car (eval.expr (cadr expr) env heap)) ;; Firt expression using the initial heap...
		(car (eval.expr (caddr expr) env 
		    (cadr (eval.expr (cadr expr) env heap)) ;; Second expression using modified heap...
		)))
		(cadr (eval.expr (caddr expr) env 
		    (cadr (eval.expr (cadr expr) env heap)) ;; Include final heap in the result.
		))
	    )]
	    [(equal? (car expr) '*) (list (* 
		(car (eval.expr (cadr expr) env heap)) ;; Firt expression using the initial heap...
		(car (eval.expr (caddr expr) env 
		    (cadr (eval.expr (cadr expr) env heap)) ;; Second expression using modified heap...
		)))
		(cadr (eval.expr (caddr expr) env 
		    (cadr (eval.expr (cadr expr) env heap)) ;; Include final heap in the result.
		))
	    )]
	    [(equal? (car expr) '/) (list (/
		(car (eval.expr (cadr expr) env heap)) ;; Firt expression using the initial heap...
		(car (eval.expr (caddr expr) env 
		    (cadr (eval.expr (cadr expr) env heap)) ;; Second expression using modified heap...
		)))
		(cadr (eval.expr (caddr expr) env 
		    (cadr (eval.expr (cadr expr) env heap)) ;; Include final heap in the result.
		))
	    )]
	    [else ERR] ;; This should never happen, cause we know our arithexpr is valid.
	)
	ERR ;; Expressions do not result in numbers, cannot evaluate.
))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Evaluation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evaluates an FExpr using the given environment.
;; \param expr	Valid FExpr to be evaluated.
;; \param env	Which environment should this be evaluated in?
;; \param heap	The heap the expression should use for evaluation.
;; \return	Pair of the form (result heap).
(define (eval.fexpr expr env heap)
    ;; Evaluate the tail expression, 
    ;; with the function definition added to the environment
    ;; Adding a function definition does not modify the heap,
    ;; but we still need to pass it through to the expression evaluation.
    (eval.expr (caddr expr) (cons (cadr expr) env) heap)
)

;; Evaluates the function application request
;; using the given environment.
;; \param expr	Valid ApplyF expression with which to be evaluated.
;; \param env	Which environment should this be evaluated in?
;; \param heap	The heap the expression should use for evaluation.
;; \return	Pair of the form (result heap).
(define (eval.applyf expr env heap)
    (eval.expr
	;; Evaluate the function body
	((get.fun.expr env) (cadr expr))
	(append ;; Environment...
	    ;; Evaluate function arguments and add them to the heap.
	    (car (eval.args 
		((get.fun.args env) (cadr expr)) 
		(cadr (cadr expr)) 
		env 
		heap))
	    ;; and using static scoping
	    ((get.fun.env env) (cadr expr)))
	;; Use the heap as it exists post function argument evaluation.
	(cadr (eval.args 
	    ((get.fun.args env) (cadr expr)) 
	    (cadr (cadr expr)) 
	    env
	    heap))
))

;; Creates an environment by evaluating each argument
;; using the given environment, and then pairing these
;; evaluations with the corresponding name in the params list.
;; Note the returned environment will only include these evaluations
;; and nothing within 'env'.
;; \param vars	Names of variables to assign evaluations to.
;; \param args	Arguments which need to be evaluated.
;; \param env	Which environment should this be evaluated in?
;; \param heap	The heap the expression should use for evaluation.
(define (eval.args params args env heap)
	(if (null? params)
	    (list '() heap) ;; Base case - nothing to do.
	    (list
		(cons
		    (list (car params) 
			(car (eval.expr (car args) env heap))) ;; Value of expression evaluation.
		    (car (eval.args (cdr params) (cdr args) env ;; Get the list of arguments...
			(cadr (eval.expr (car args) env heap))))) ;; using teh heap post current evaluation.
		;; Get the heap state at the end of recursion.
		(cadr (eval.args (cdr params) (cdr args) env 
		    (cadr (eval.expr (car args) env heap))))
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditional Evaluation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Takes a conditional expression of form (CCond Expr1 Expr2)
;; evaluates CCond to TRUE or FALSE,
;; if true, returns the evaluation of Expr1, else if false
;; returns the evaluation of Expr2.
(define (eval.condexpr expr env heap)
    ;; Evaluate the given conditional in our environment.
    (if (car (eval.ccond (car expr) env heap))
	;; If TRUE, evaluate the first expression...
	(eval.expr (cadr expr) env 
	    (cadr (eval.ccond (car expr) env heap))) ;; with the new heap...
	;; else evaluate the second
	(eval.expr (caddr expr) env 
	    (cadr (eval.ccond (car expr) env heap))) ;; with the new heap.
))

;; Evaluates a conditional expression to TRUE or FALSE.
;; \param expr	Valid conditional expression to be evaluated.
;; \param env	The environment with which to evaluate.
;; \return	A pair of the form (result heap) where result is either True or False
(define (eval.ccond expr env heap)
    (cond
	[(equal? (car expr) 'or)
	    (list (or
		(car (eval.ccond (cadr expr) env heap))
		(car (eval.ccond (caddr expr) env 
		    (cadr (eval.ccond (cadr expr) env heap)) ;; Use the heap post evaluation of Expr1.
		)))
	    ;; Include the heap post evaluation of Expr2.
	    (cadr (eval.ccond (caddr expr) env 
		(cadr (eval.ccond (cadr expr) env heap)) ;; Use the heap post evaluation of Expr1.
	    ))
	)]
	[(equal? (car expr) 'and)
	    (list (and
		(car (eval.ccond (cadr expr) env heap))
		(car (eval.ccond (caddr expr) env 
		    (cadr (eval.ccond (cadr expr) env heap)) ;; Use the heap post evaluation of Expr1.
		)))
	    ;; Include the heap post evaluation of Expr2.
	    (cadr (eval.ccond (caddr expr) env 
		(cadr (eval.ccond (cadr expr) env heap)) ;; Use the heap post evaluation of Expr1.
	    ))
	)]
	[(equal? (car expr) 'not)
	    (list (not
		;; Evaluate the ccond, and negate the 'result'.
		(car (eval.ccond (cadr expr) env heap)))
	    ;; Include modifications to the heap.
	    (cadr (eval.ccond (cadr expr) env heap))
	)]
	[else (eval.bcond expr env heap)]
))

;; Takes an input binary conditional expression (gt, lt, eq), evaluates
;; both parameters in the input environment, then returns teh result of the
;; conditional on the two results.
;; \param expr	Binary expression to evaluate.
;; \param env	Environment in which to evaluate parameters.
;; \return	A pair of the form (result heap).
;;		Where result is True if the conditional evaluates to True, False otherwise.
(define (eval.bcond expr env heap)
    (cond
	[(equal? (car expr) 'gt) 
	    (list (> ;; Find the result...
		(car (eval.expr (cadr expr) env heap))
		(car (eval.expr (caddr expr) env 
		    (cadr (eval.expr (cadr expr) env heap)) ;; Use the heap from evaluation of Expr1.
		)))
	    ;; and pair it with the heap from evaluation of Expr2.
	    (cadr (eval.expr (caddr expr) env 
		(cadr (eval.expr (cadr expr) env heap)) ;; Use the heap from evaluation of Expr1.
	    ))
	)]
	[(equal? (car expr) 'lt) 
	    (list (< ;; Find the result...
		(car (eval.expr (cadr expr) env heap))
		(car (eval.expr (caddr expr) env 
		    (cadr (eval.expr (cadr expr) env heap)) ;; Use the heap from evaluation of Expr1.
		)))
	    ;; and pair it with the heap from evaluation of Expr2.
	    (cadr (eval.expr (caddr expr) env 
		(cadr (eval.expr (cadr expr) env heap)) ;; Use the heap from evaluation of Expr1.
	    ))
	)]
	[(equal? (car expr) 'eq) 
	    (list (equal? ;; Find the result...
		(car (eval.expr (cadr expr) env heap))
		(car (eval.expr (caddr expr) env 
		    (cadr (eval.expr (cadr expr) env heap)) ;; Use the heap from evaluation of Expr1.
		)))
	    ;; and pair it with the heap from evaluation of Expr2.
	    (cadr (eval.expr (caddr expr) env 
		(cadr (eval.expr (cadr expr) env heap)) ;; Use the heap from evaluation of Expr1.
	    ))
	)]
	[else ERR]
))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable Evaluation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Given an expression of the form (var VarAssign Expr), creates a new environment
;; by appending the results of VarAssign, this new environment is then
;; used to evaluate Expr, whose result will be returned.
(define (eval.varexpr expr env heap)
    ;; Evaluate the expression in our new environment.
    (eval.expr 
	(caddr expr) 
	(car (eval.varassign (cadr expr) env heap))
	(cadr (eval.varassign (cadr expr) env heap))
))

;; Given a list of variable assignments (as a list of orderd pairs), 
;; return a new environment created with new variable assignments appended.
;; If a variable is given by an expression, that expression will be evaluated
;; and its numerical value will be stored in the environment (eager evaluation).
;; \param expr	List of variable assignment sequences.
;; \param env	Used to evaluate varassign expressions, new variables
;;		will be appended to this environment in the return value.
;; \return	Environment/Heap combination after variable assignments have been completed.
(define (eval.varassign expr env heap)
    (if (null? expr)
	(list env heap)	;; Base case - done constructing the environment.
	(eval.varassign 
	    (cdr expr)				;; Evaluate the remaining variables
	    (car  (eval.varassignment (car expr) env heap)) ;; in our new environment,
	    (cadr (eval.varassignment (car expr) env heap)) ;; with the modified heap.
)))

;; Takes a single variable assignment as an orderd pair
;; in the form of (Variable Expr) and evaluates 'Expr'.
;; Returns a pair of the form (env heap), where env
;; is the expanded input environment to include the new variable,
;; and heap includes any modifications made to the heap by expr.
(define (eval.varassignment expr env heap)
    (if (number? (car (eval.expr (cadr expr) env heap)))
	(list
	    (cons ;; Create the new environment,
		(list (car expr) (car (eval.expr (cadr expr) env heap))) ;; but add the new variable.
		env)
	    (cadr (eval.expr (cadr expr) env heap))) ;; Include the modified heap.
	ERR
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
		    ;; The environment may contain variables and function definitions.
		    (or (env.is.var? (car env)) (fassign? (car env)))
		    ;; The remaining elements must also be a valid.
		    (env.valid? (cdr env))
	    ))
	    #F	;; An environment must be a list by definition.
)))

;; Checks whether or not the given environment element is a variable.
(define env.is.var?
    (lambda (elem)
	(and
	    (list? elem)		;; First element must be a list
	    (equal? (length elem) 2)	;; containing two elements.
	    (variable? (car elem))	;; The first is the variable definition.
	    (number? (cadr elem))	;; Second must be a number (eager evaluation).
)))

;; Checks whether or not the environment contains the given variable,
;; the variable is treated as a key into the set of ordered pairs in the environment.
;; Returns TRUE if the var is found as a key in the environment, FALSE otherwise.
;; \param env The environment to search through.
;; \param var The kep we are looking for in env.
(define env.contains.var?
    (lambda (env)
	(lambda (var)
	    (if (and (list? env) (not (null? env)))
		(if (equal? (car (car env)) var)	;; (always fails if function is found)
		    #T					;; Found the variable,
		    ((env.contains.var? (cdr env)) var)	;; else keep recursing.
		)
		#F					;; Base case, variable not found.
))))

;; Returns the environment in which the given function application should be evaluated.
;; This will be everything in the environment which follows the function definition.
;; \param env	The environment we are looking in.
;; \param fun	Function application we would like to know if is defined or not.
;;		Should be of the form (FName (ARG0 ARG1 ...)).
(define get.fun.env
    (lambda (env)
	(lambda (fun)
	    (if (and (list? env) (not (null? env)))
		;; Does the front function have the correct function name
		;; and the correct number of parameters?
		(if (and
		    ;; Is this a function?
		    (fassign? (car env))
		    ;; Do the function names match?
		    (equal? (car (car (car env))) (car fun))
		    ;; Do the number of parameters match?
		    (equal? (length (cadr fun)) (length (cadr (car (car env))))))
			;; Found the function we are looking for,
			env ;; so return the rest of the environment.
			    ;; include the current function definition 
			    ;; so recursion is supported.
			;; else keep recursing.
			((get.fun.env (cdr env)) fun))
		'() ;; Base case, function not found.
))))

;; Searches the given environment for the given function.
;; This function will then return that functions definition.
;; function signature.
;; \param env	The environment we are looking in.
;; \param fun	Function application we would like to know if is defined or not.
;;		Should be of the form (FName (ARG0 ARG1 ...)).
(define get.fun
    (lambda (env)
	(lambda (fun)
	    (if (and (list? env) (not (null? env)))
		;; Does the front function have the correct function name
		;; and the correct number of parameters?
		(if (and
		    ;; Is this a function?
		    (fassign? (car env))
		    ;; Do the function names match?
		    (equal? (car (car (car env))) (car fun))
		    ;; Do the number of parameters match?
		    (equal? (length (cadr fun)) (length (cadr (car (car env))))))
			;; Found the function we are looking for,
			(car env) ;; so return the function.
			;; else keep recursing.
			((get.fun (cdr env)) fun)
		) ERR ;; Base case, function not found.
))))

;; Returns the function body for the requested function in the given environment.
;; \param env	The environment we are looking in.
;; \param fun	Function application we would like to know if is defined or not.
;;		Should be of the form (FName (ARG0 ARG1 ...)).
(define get.fun.expr
    (lambda (env)
	(lambda (fun)
	    (cadr ((get.fun env) fun))
)))

;; Returns the list of argument names for the requested function in the given environment.
;; \param env	The environment we are looking in.
;; \param fun	Function application we would like to know if is defined or not.
;;		Should be of the form (FName (ARG0 ARG1 ...)).
(define get.fun.args
    (lambda (env)
	(lambda (fun)
	    (cadr (car ((get.fun env) fun)))
)))

;;;;;;;;;;;;;;;;;;;;;
;; Syntax Checking ;;
;;;;;;;;;;;;;;;;;;;;;

;; Is the input expression valid?
;; \param e	The expression to check.
(define (expr? e)
    (or
	(number? e)	;; Either a Number
	(variable? e)	;; or a Variable
	(opexpr? e)	;; or an OpExpr
	(fexpr? e)	;; or an FExpr
	(applyf? e)	;; or ApplyF
	(dref? e)	;; or DRef
	(wref? e)	;; or WRef
	(ref? e)	;; or Ref (ref)
	(free? e)	;; or Ref (free).
))

;; Is the input operator expression valid?
;; \param e Operator expression to check.
(define (opexpr? e)
    (or
	(arithexpr? e)	;; Either an ArithExpr
	(condexpr? e)	;; or CondExpr
	(varexpr? e)	;; or VarExpr.
))

;; Is the input arithmetic expression valid?
;; \param e Arithmetic expression to check.
(define (arithexpr? e)
    (and
	(list? e)		;; Must be a list
	(equal? (length e) 3)	;; of length 3.
	(op? (car e))		;; First element is an operator,
	(expr? (cadr e))	;; followed by the first operand
	(expr? (caddr e))	;; followed by the second operand.
))

;; Is the given input an operator?
;; \param o Operator to check.
(define (op? o)
    (or 
	(equal? o '+)
	(equal? o '-)
	(equal? o '*)
	(equal? o '/)
))


;; Is the input expression a conditional expression?
;; \param c Expression to check.
(define (condexpr? c)
    (and
	(list? c)		;; Must be a list
	(equal? (length c) 3)	;; of length 3.
	(ccond? (car c))	;; First element is a CCond,
	(expr? (cadr c))	;; followed by an expression evaluated on true
	(expr? (caddr c))	;; and an expression evaluated on false.
))

;; Check whether or not the input CCond can be considered
;; an or, and, not, or binary conditional.
;; \param c Expression to be checked.
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

;; Check whether or not the input conditional is a binary conditional.
;; \param b The expression to check.
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

;; Checks whether the input item is a variable expression.
;; \param Input item to check.
(define (varexpr? v)
    (if (list? v)
	(if (equal? (length v) 3)
	    (and (and
		(equal? (car v) 'var)			;; A variable expression is of the form (var
		(list? (cadr v))			;; followed by a list VarAssign
		(not (equal? (length (cadr v)) 0))	;; with at least one assignment in it
		(varassign? (cadr v))			;; that is syntactically valid
		(expr? (caddr v))			;; followed by an expression.
	    ))
	    #F 				;; with nothing else in it.
	)
	#F				;; It certainly at least has to be a list.
))

;; Checks whether or not the input variable assignment is valid.
;; \param v Variable assignment to check.
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

;; Is the input pair of the form (Variable Expr)
;; \param v Ordered pair to check.
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

;; Is the input item a valid symbol that may be used 
;; as a variable name?
;; \param v Candidate for variable name.
(define (variable? v)
    (symbol? v)
)

;; Is the input item a valid function name?
;; Currently not defined in the homework assignment
;; I assume it is a symbol, but this may need to be changed.
;; \param f Candidate for function name.
(define (fname? f)
    (symbol? f)
)

;; Is the input function expression valid?
;; Check that f defines a single function, then provides an 
;; expression to be evaluated given that functions definition.
;; \param f Candidate function expression.
(define (fexpr? f)
    (and
	(list? f)		;; Needs to be a list
	(equal? (length f) 3)	;; of length 3
	(equal? (car f) 'fun)	;; starting with the identifier 'fun'
	(fassign? (cadr f))	;; followed by a function assignment
	(expr? (caddr f))	;; closed by an expression.
))

;; Is the input function assignment valid?
;; \param f The function assignment to check.
(define (fassign? f)
    (and
	(list? f)			;; Must be a list
	(equal? (length f) 2)		;; With two elements
	(list? (car f))			;; the first is another list
	(equal? (length (car f)) 2)	;; of length 2
	(fname? (car (car f)))		;; containing the function name
	(formalparams? (cadr (car f)))	;; and the formal arguments
	(expr? (cadr f))		;; quanitity followed by the
					;; function definition as an expression.
))

;; Is the input a list a formal function parameters?
;; Note these are not type, simply a list of variable names
;; that the function expects.
;; \param f Candidate for the list of function parameters.
(define (formalparams? f)
    (if (null? f)
	#T	;; Recursion base case.
	(and
	    (list? f)			;; Must eb a list
	    (variable? (car f))		;; with a variable at the front...
	    (formalparams? (cdr f))	;; and variables in the tail.
)))

;; Is the input function application valid?
;; \param a Function application to test.
(define (applyf? a)
    (and
	(list? a)			;; Must be a list 
	(equal? (length a) 2)		;; of length two
	(equal? (car a) 'apply)		;; starting with 'apply'
	(list? (cadr a))		;; followed by a list
	(equal? (length (cadr a)) 2)	;; with two items
	(fname? (car (cadr a)))		;; the first is the function name
	(args? (cadr (cadr a)))		;; and the second is the list of arguments.
))

;; Is the input item a valid argument list?
;; An argument list is simply just a list of expressions
;; which are evaluated to be the function arguments.
;; \param a Argument list to check.
(define (args? a)
    (if (null? a)
	#T	;; Recursion base case.
	(and
	    (list? a)		;; Must be a list
	    (expr? (car a))	;; with an expression at the front...
	    (args? (cdr a))	;; and expressions in the tail.
)))

;; Checks if the input item is a valid derefence.
(define (dref? d)
    (and
	(list? d)
	(equal? (length d) 2)
	(equal? (car d) 'deref)
	(expr? (cadr d))
))

;; Checks if the input item is a valid write reference.
(define (wref? w)
    (and
	(list? w)
	(equal? (length w) 3)
	(equal? (car w) 'wref)
	(expr? (cadr w))
	(expr? (caddr w))
))

;; Checks if the input item is a valid reference allocator.
(define (ref? r)
    (and
	(list? r)
	(equal? (length r) 2)
	(equal? (car r) 'ref)
	(expr? (cadr r))
))

;; Checks if the input item is a valid reference deallocator.
(define (free? r)
    (and
	(list? r)
	(equal? (length r) 2)
	(equal? (car r) 'free)
	(expr? (cadr r))
))
