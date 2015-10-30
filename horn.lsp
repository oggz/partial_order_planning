; Logical Reasoning in Horn Clause Knowledge Bases
;(ld "prop.lsp")

(define-class horn-kb
	(ivars table)
)

(define-method (horn-kb 'initialize)
	(set! table nil)
	self
)

; Add a sentence to a Horn kb.
(define-method  (horn-kb 'tell sentence)
    (let ((horn (->horn sentence)))
	(dotimes (i (length horn))
		(let ((clause (list-ref horn i)))
			(self 'insert-table clause)
		)
	)
    )
    'ok
)

(define (->horn sent)
	(list (logic sent)))

(define-method (horn-kb 'insert-table clause)
	(let* ((rhs (get-rhs clause))
	       (match (assoc rhs table)))
		(if match
		   (begin 
			(self 'remove-table match)
			(set! table (cons  
					(cons rhs (cons clause (cdr match)))
					table))
		   )
		   (set! table (cons (cons rhs (list clause))
			 	     table))
		)
	)
)

(define-method (horn-kb 'remove-table match)
	(define (remove match tab)
		(cond ((null? tab) nil)
		      ((equal? (car tab) match) (cdr tab))
		      (else (cons (car tab) (remove match (cdr tab))))
		)
	)
	(set! table (remove match table))
)

(define (get-rhs clause)
	(cond ((atom? clause) clause)
	      ((equal? (car clause) 'implies) (get-rhs (caddr clause)))
	      (else (car clause))
	)
)

; Use backward chaining to decide if sentence is true
(define-method (horn-kb 'ask-each query fn)
 (let ((answer (back-chain-each self query +no-bindings+ fn)))
answer
))

(define-method (horn-kb 'ask query)
   (let ((vars (variables-in query)))
	(if vars
		(self 'ask-each (list (logic query)) (lambda(binding) 
						(list (get-vars binding vars))))
		(if (self 'ask-each (list (logic query)) (lambda(x) x))
			'T
			nil
		)
	)
   )
)

(define (get-vars binding vars)
	(map (lambda(var) (list var (get-var binding var))) vars))

(define (get-var binding var)
	(let ((val (cdr (assoc var binding))))
		(if (variable? val)
			(get-var binding val)
			val
		)
	)
)

; Solve the conjunction of goals by backward chaining
(define (back-chain-each kb goals bindings fn)
  (cond ((equal? bindings +fail+) +fail+)
	((null? goals)  (fn bindings))
	(else (let ((goal (car goals)))
		(cond ((equal? goal 'F) +fail+)
		      ((equal? goal 'T) 
			  (back-chain-each kb (cdr goals) bindings fn))
		      ((equal? (car goal) '=) 
			  (back-chain-each kb (cdr goals) 
				(unify (cadr goal) (caddr goal) bindings)
				fn))
		      ((equal? (car goal) 'and)
			  (back-chain-each kb (append (conjuncts goal) 
						      (cdr goals))
				bindings fn))
		      ((equal? (car goal) 'or))
		      ((equal? (car goal) 'not)  ; use refutation
			(if (back-chain-each kb (list (cadr goal)) bindings fn)
				+fail+
				(back-chain-each kb (cdr goals) bindings fn))) 
		      (else ; Look at all clauses that could conclude the goal
			(let ((clauses (cdr (assoc (car goal) 
						(kb 'get-variable 'table))))
			      (answer nil))
			    (dotimes (i (length clauses))
				(let ((clause (rename-variables 
						(list-ref clauses i))))
				   (if (equal? (car clause) 'implies)
				      (set! answer (append
					(back-chain-each kb
						(append (conjuncts
								(cadr clause))
							(cdr goals))
						(unify goal (caddr clause)
							bindings)
						fn) answer))
				      (set! answer (append
					(back-chain-each kb (cdr goals)
						(unify goal clause bindings)
						fn) answer))
				   )
				)
			    )
			    answer
			)
		      )
		)
	      )
	)
   )
)

(define (conjuncts sent)
	(cond ((atom? sent) (list sent))
	      ((equal? (car sent) 'and) 
		(accumulate append (map conjuncts (cdr sent))))
	      (else (list sent))))

;_________________________________________________________________________
; Unification and Substitutions (aka Binding Lists)


(define +fail+ nil) 	; indicaties unification failure

(define +no-bindings+ '((nil))) ; indicates unification success, with no vars

; Top Level Functions

; See if x and y match with given bindings.  Return binding list that would
; make them equal
(define (unify x y &optional (bindings +no-bindings+))
  (cond ((equal? bindings +fail+) +fail+)
        ((equal? x y) bindings)
        ((variable? x) (unify-var x y bindings))
        ((variable? y) (unify-var y x bindings))
        ((and (pair? x) (pair? y))
         (unify (cdr x) (cdr y) 
                (unify (car x) (car y) bindings)))
        (else +fail+)))

; Replace all variables in x with new ones
(define (rename-variables x &optional (bindings nil))
   (if (null? bindings)
     (let* ((vars (variables-in x))
	   (bindings (map (lambda(var) (make-binding var (new-variable var)))
			vars)))
  	(map (lambda (y) (cond ((variable? y)  (assoc y bindings))
			       ((list? y) (rename-variables y bindings))
			       (else y)))
          x)
     )
     (map (lambda (y) (cond ((variable? y)  (cdr (assoc y bindings)))
			       ((list? y) (rename-variables y bindings))
			       (else y)))
          x)
   )
)

; Auxiliary Functions

; Unify var with x, using and maybe extending bindings
(define (unify-var var x bindings)
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((and (variable? x) (get-binding x bindings))
         (unify var (lookup x bindings) bindings))
        ((occurs-in? var x bindings)
         +fail+)
        (else (extend-bindings var x bindings))))

; Is x a variable (a symbol starting with $)?
(define (variable? x)
   (and (symbol? x) (equal? "$" (substring (symbol->string x) 0 1))))

; Find a (variable . value) pair in a binding list
(define (get-binding var bindings)
  (assoc var bindings))

; Get the variable part of a single binding
(define (binding-var binding)
  (car binding))

; Get the value part of a single binding
(define (binding-val binding)
  (cdr binding))

(define (make-binding var val) (cons var val))

; Get the value part (for var) from a binding list
(define (lookup var bindings)
  (binding-val (get-binding var bindings)))

; Add a (var . value) pair to a binding list
(define (extend-bindings var val bindings)
  (cons (make-binding var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy +no-bindings+
        (if (equal? bindings +no-bindings+)
            nil
            bindings)))

; Does var occur anywhere inside x?
(define (occurs-in? var x bindings)
  (cond ((equal? var x) #t)
        ((and (variable? x) (get-binding x bindings))
         (occurs-in? var (lookup x bindings) bindings))
        ((pair? x) (or (occurs-in? var (car x) bindings)
                       (occurs-in? var (cdr x) bindings)))
        (else nil)))

; Substitute the value of variables in bindings into x, taking recursively
; bound variables into account
(define (subst-bindings bindings x)
  (cond ((equal? bindings +fail+) +fail+)
        ((equal? bindings +no-bindings+) x)
        ((and (variable? x) (get-binding x bindings))
         (subst-bindings bindings (lookup x bindings)))
        ((atom? x) x)
        (else (cons (subst-bindings bindings (car x))
                    (subst-bindings bindings (cdr x)))
         )))

; Return something that unifies with both x and y (or fail).
(define (unifier x y)
 (subst-bindings (unify x y) x))

; Return a list of all the variables in exp
(define (variables-in exp)
  (unique-find-anywhere-if (lambda(x)(variable? x)) exp))

; Return a list of leaves of tree satisfying the predicate with
; duplicates removed
(define (unique-find-anywhere-if predicate tree &optional found-so-far)
  (if (atom? tree)
      (if (and (predicate tree) (not (member? tree found-so-far)))
          (cons tree found-so-far)
          found-so-far)
      (unique-find-anywhere-if
        predicate
        (car tree)
        (unique-find-anywhere-if predicate (cdr tree)
                                 found-so-far))))

; Does predicate apply to any atom in the tree?
(define (find-anywhere-if predicate tree)  
  (if (atom? tree)
      (predicate tree)
      (or (find-anywhere-if predicate (car tree))
          (find-anywhere-if predicate (cdr tree)))))

(define *new-variable-counter* 0)

; Create a new variable.  Assumes user never types variables of form $X.9
(define (new-variable var)
  (set! *new-variable-counter* (+ 1 *new-variable-counter*))
  (string->symbol 
	(string-append (if (variable? var) "" "$")
                 (symbol->string var) "." 
		 (number->string *new-variable-counter*))))







