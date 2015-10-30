; Neural Networks
(ld "learn.lsp")

; A network is represented as a list of lists of units
; Inputs assumed to be the ordered attribute values in examples
; Every unit gets input 0 set to -1

(define-class unit
	(ivars	parents		; seq. of indices of units in previous layer
		children	; seq. of indices of units in next layer
		weights		; weights on links from parents
		g		; activation function
		dg		; activation gradient function g'
		a		; activation level
		in		; total weighted input
		gradient	; g'(in_i)
	)
)

(define-method (unit 'initialize &key (parents nil) (children nil)
				      (weights nil) (g sigmoid)
				      (dg (lambda(x)(* (g x) (- 1 (g x))))))
	(self 'set-variable! 'parents parents)
	(self 'set-variable! 'children children)
	(self 'set-variable! 'weights weights)
	(self 'set-variable! 'g g)
	(self 'set-variable! 'dg dg)
	self
)

(define (sigmoid x)
	(/ 1 (1+ (exp (- x)))))

(define (step-function threshold x)
	(if (> x threshold) 1 0))

(define (sign-function threshold x)
	(if (> x threshold) 1 -1))

; coded examples have goal values (in a single list) followed by attribute
; values, both in fixed order
(define (code-examples examples attributes goals)
	(map (lambda(e) (code-example e attributes goals)) examples))

(define (code-example example attributes goals)
	(cons (map (lambda(g) (attribute-value g example)) goals)
	      (map (lambda(a) (attribute-value a example)) attributes)
	)
)

(define (nn-learning problem network learning-method
		&key (tolerance (* .01 (length 
					(problem 'get-variable 'examples))))
		     (limit 1000))
   (let* ((examples (problem 'get-variable 'examples))
	  (attributes (problem 'get-variable 'attributes))
	  (goals (problem 'get-variable 'goals)) 
	  (coded-examples (code-examples examples attributes goals)))

	(define (nn-loop epoch network)
	  (let ((all-correct #t) 
		(error (nn-error coded-examples network))
		(nw network))
;(print ((car nw) 'get-variable 'weights))
;(print epoch) (display " ") (display error)
	      (if (< epoch limit)
		(if (< error tolerance) 
			nw
			(begin
			    (dotimes (i (length coded-examples))
				(let* ((e (list-ref coded-examples i))
				       (target (car e))
				       (predicted (network-output (cdr e) nw)))
					(set! all-correct (and all-correct
							     (equal? target
								    predicted)))
					(set! nw (learning-method nw (cdr e)
							predicted target))
				)
			     )
			     (if all-correct
				 nw
				 (nn-loop (+ 1 epoch) nw)
			      )
			)
		)
		network
	     )
	   )
	)

	(nn-loop 0 network)
   )
)

(define (square x) (* x x))

(define (nn-error examples network)
   (let ((sum 0))
	(dotimes (i (length examples))
		(let* ((e (list-ref examples i))
		       (target (car e))
		       (predicted (network-output (cdr e) network)))
			(map (lambda (x y) (set! sum (+ sum (square (- x y)))))
			     predicted target)
		)
	)
	sum
   )
)

(define (network-output inputs network)
    (let ((in inputs))
	(dotimes (i (length network))
	    (let ((layer network i))
		(set! in 
		     (map (lambda(unit) (unit-output (get-unit-inputs in
							  (unit 'get-variable
								'parents))
						      unit))
			  layer
		      )
		)
	     )
	)
	in
    )
)

; unit-output computes the output of a unit given a set of inputs
; it always adds a bias input of -1 as the zeroth input
(define (unit-output inputs unit)
   (let ((g (unit 'get-variable 'g))
	 (dg (unit 'get-variable 'dg))
  	 (weights (unit 'get-variable 'weights)))
	(unit 'set-variable! 'in (dot-product weights (cons -1 inputs)))
	(unit 'set-variable! 'gradient (dg (unit 'get-variable 'in)))
	(unit 'set-variable! 'a (g (unit 'get-variable 'in)))
	(unit 'get-variable 'a)
   )
)

(define (dot-product l1 l2)
   (let ((sum 0))
	(map (lambda (x1 x2) (set! sum (+ sum (* x1 x2)))) l1 l2)
	sum
    )
)

(define (get-unit-inputs inputs parents)
	(map (lambda(parent) (list-ref inputs parent)) parents)
)


;________________________________________________________________________
; perceptron learning - single-layer neural networks

; make-perceptron returns a one-layer network with m units, n inputs each

(define (make-perceptron n m &optional (g (lambda(i)(step-function 0 i))))
    (if (<= m 0)
	nil
	(cons (unit 'new :parents (iota (1+ n))
		         :weights (random-weights (1+ n) -.5 +.5)
			 :g g)
	      (make-perceptron n (- m 1) g)
	)
    )
)

(define (iota n &optional (start-at 0))
	(if (<= n 0) nil (cons start-at (iota (- n 1) (+ start-at 1)))))

(define (random-weights n low high)
    (if (<= n 0)
	nil
	(cons (+ low (* (/ (random (round 100)) 100.0) (- high low)))
	      (random-weights (- n 1) low high)
	)
    )
)

(define (perceptron-learning problem)
	(nn-learning problem 
		    (make-perceptron (length (problem 'get-variable 
						      'attributes))
				     (length (problem 'get-variable 'goals))
		    )
		    perceptron-update
	)
)

(define (perceptron-update perceptron actual-inputs predicted target
		&optional (learning-rate .1))
   (let ((all-inputs (cons -1 actual-inputs)))
	(map (lambda(unit predicted_i target_i)
		(unit 'set-variable! 'weights
			(map (lambda(weight input)
				(+ weight (* learning-rate 
					     input
					     (- target_i predicted_i))))
			     (unit 'get-variable 'weights)
			     all-inputs
			)
		)
		unit)
		perceptron predicted target
	)
    )
)

(define (print-nn network)
	(print (cons 'inputs (iota (length 
				((caar network) 'get-variable 'weights)))))
	(dotimes (i (length network))
	   (let ((layer (list-ref network i)))
		(print 'layer)
		(dotimes (j (length layer))
		   (let ((unit (list-ref layer j)))
			(display "   ") (display (list 'unit (+ j 1)
						       'weights))
			(display (unit 'get-variable 'weights))
		   )
		)
	   )
	)
)

(define (nn-performance-element h e attributes goals)
	(network-output (cdr (code-example e attributes goals)) h))

(define (nn-algorithm training-set attributes goals)
	(perceptron-learning
		(learning-problem 'new :examples training-set
				:attributes attributes :goals goals))
)

