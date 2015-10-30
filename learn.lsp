;Definitions for learning problems

(define-class learning-problem
	(ivars examples
	       attributes
	       goals))

(define-method (learning-problem 'initialize &key (examples nil)
						  (attributes nil)
						  (goals nil))
	(self 'set-variable! 'examples examples)
	(self 'set-variable! 'attributes attributes)
	(self 'set-variable! 'goals goals)
	self
)

(define (attribute-name attribute) (car attribute))
(define (attribute-values attribute) (cdr attribute))
(define (attribute-value attribute example)
	(cdr (assoc (attribute-name attribute) example)))

;____________________________________________________________________________
; generate random examples of a problem

(define (random-element lst)
	(list-ref lst (random (length lst))))

; generate n random examples
(define (random-examples n attributes)					   
	(if (= n 0)
		nil
		(cons (map (lambda(a)
				(cons (attribute-name a)
				      (random-element (attribute-values a))))
			   attributes)
		      (random-examples (- n 1) attributes)
		)
	)
)

;_____________________________________________________________________________
; test the learning method

; classify test set using hypothesis h and performance element which takes the
; hypothesis + example -> prediction

(define (classify unclassified-examples goals h performance-element)
	(map (lambda(e) (append 
			  (map (lambda(goal value) 
					(cons (attribute-name goal) value))
				goals
				(performance-element h e)
			  )
			  e))
	     unclassified-examples
	)
)

; compute the learning curve
(define (learning-curve algorithm performance-element
		examples attributes goals trials increment)
   (let ((training-set-size 0)
	 (points (- (floor (/ (length examples) increment)) 1))
	 (results nil) (test-set nil) (training-set nil) (count 0))
	(dotimes (i points)
		(set! training-set-size (+ training-set-size increment))
		(set! count 0)
		(dotimes (trial trials)
			(set! training-set (sample training-set-size
						   examples))
			(set! test-set 
				(keep (lambda(e) (not (member? e 
							  training-set)))
				      examples))
			(set! count (+ count 
				      (accuracy (algorithm training-set
							attributes goals)
						performance-element
						test-set attributes goals)))
		)
		(set! results (cons (cons training-set-size 
					 (/ count trials))
				    results))
	)
	(reverse results)
   )
)

(define (accuracy h performance-element test-set attributes goals)
	(/ (accumulate + (map (lambda(e)
				(if (equal? (performance-element h e
						attributes goals)
					    (map (lambda(g)
						   (attribute-value g e))
						 goals)
					    )
					    1
					    0
				)
			)
			test-set)
	    )
	    (length test-set))
)

; sample without replacement	
(define (sample n population)
	(let ((m (length population)))
		(cond ((<= n 0) nil)
			((>= n m) population)
			((>= (/ n m) (/ (random 100) 100))
				(cons (car population) 
					(sample (- n 1) (cdr population))))
			(else (sample n (cdr population)))
		)
	)
)




