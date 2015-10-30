; decision tree learning
; returns a tree in the format
; (a1 (v11 . <tree>) (v12 . <tree>)), bottoming out with goal values.
; handling only a single goal attribute
(ld "learn.lsp")

(define (decision-tree-learning problem)
	(dtl (problem 'get-variable 'examples)
	     (problem 'get-variable 'attributes)
	     (problem 'get-variable 'goals)))

(define (dtl examples attributes goals &optional prior)
   (let ((goal (car goals)))
	(cond ((null? examples) prior)
	      ((null? attributes) (majority examples goal))
	      ((all-same? examples goal) (majority examples goal))
	      (else
		(let ((best (select-attribute examples attributes
					      goal)))
			(list (car best)
			      (map (lambda(v)
				     (cons v 
					  (dtl (get-examples examples
							best v)
					       (remove-attribute
						  attributes best)
					       (list goal)
					       (majority examples
							 goal))
				     ))
				    (cdr best)
			      )
			)
		)
	      )
	)
   )
	
)

(define (dt-predict dt example)
	(if (atom? dt)
		(list dt)
		(dt-predict (cdr (assoc (cdr (assoc (car dt) example))
					(cadr dt)))
				   example)
	)
)

(define (test-dtl problem n)
	(classify (random-examples n (problem 'get-variable 'attributes))
		  (problem 'get-variable 'goals)
		  (decision-tree-learning problem)
		  dt-predict
	)
)

(define (all-same? examples goal)
	(let ((first-value (attribute-value goal (car examples))))
	   (=   (length (cdr examples))
	   	(length (keep (lambda(example) 
				(equal? (attribute-value goal example)
					first-value
			  	))
		              (cdr examples))
	   	)
	   )
	)
)

; return examples with best = value
(define (get-examples examples best value)
	(keep (lambda(example)(equal? (attribute-value best example)
				      value))
	      examples)
)

; remove attribute from attribute list
(define (remove-attribute attributes attr)
	(cond ((null? attributes) nil)
	      ((equal? (caar attributes) (attribute-name attr))
		(cdr attributes))
	      (else (cons (car attributes) 
			  (remove-attribute (cdr attributes) attr)))
	)
)

(define (the-biggest fn lst &optional (biggest nil) (best-val nil))
	(cond ((null? lst) biggest)
	      ((null? biggest) 
			(the-biggest fn (cdr lst) (car lst) (fn (car lst))))
	      ((> (fn (car lst)) best-val)
			(the-biggest fn (cdr lst) (car lst) (fn (car lst))))
	      (else (the-biggest fn (cdr lst) biggest best-val))
	)
)

(define (count-vals value lst)
  	(cond ((null? lst) 0)
	      ((equal? (car lst) value) (+ 1 (count-vals value (cdr lst))))
	      (else (count-vals value (cdr lst)))
	)
)

(define (majority examples goal)
   (the-biggest (lambda(v) (count-vals v 
				  (map (lambda(e) (attribute-value goal e))
				       examples)
			   ))
		(cdr goal)
   )
)

(define (select-attribute examples attributes goal)
   (the-biggest (lambda(a)(information-value a examples goal))
		attributes)
)

(define (distribution examples goal)
   (define (distr values)
	(if (null? values)
	    nil
	    (cons (/ (count-attr-values examples goal (car values))
		     (max (length examples) 1))
		  (distr (cdr values)))
	)
   )
   (distr (cdr goal))
)

(define (count-attr-values examples attribute value)
	(cond ((null? examples) 0)
	      ((equal? (attribute-value attribute (car examples))
			value)
		   (+ 1 (count-attr-values (cdr examples) attribute value)))
	      (else (count-attr-values (cdr examples) attribute value))
	)
)

(define (bits-required distr)
	(accumulate + (map (lambda(p)(if (= p 0) 
					0 
					(* (- p) (/ (log p) (log 2))))) 
			   distr)))
	      
	
(define (information-value a examples goal)
   (let* ((i (bits-required (distribution examples goal)))
	  (values (cdr a))
	  (ex-lst (map (lambda(v)(keep (lambda(e)(equal? (attribute-value
								a e)
							 v))
					examples))
			values)))
	(- i (accumulate + (map (lambda(exs)(* (/ (length exs) 
						  (length examples))
					       (bits-required
						  (distribution exs goal))))
				ex-lst))
	)
   )
)
