; Partial Order Planner
(ld "horn.lsp")
(ld "queue.lsp")

(define-class plan-class
	(ivars 	steps		; a list of steps
		ordering	; the partial ordering
		bindings	; bindings of variables
		links		; the links between steps
		subgoals	; the list of remaining subgoals
	)
)

(define-method (plan-class 'initialize &key (steps nil) (ordering nil)
		(bindings +no-bindings+) (links nil) (subgoals nil))
	(self 'set-variable! 'steps steps)
	(self 'set-variable! 'ordering ordering)
	(self 'set-variable! 'bindings bindings)
	(self 'set-variable! 'links links)
	(self 'set-variable! 'subgoals subgoals)
	self
)

(define-class operator
	(ivars	action		; the operator action
		precond		; the precondition
		effect-add	; the added propositions of the action
		effect-delete	; propositions deleted by the action
	)
)

(define-method (operator 'initialize &key (action nil) (precond nil)
					  (effect-add nil) (effect-delete nil))
	(self 'set-variable! 'action action)
	(self 'set-variable! 'precond precond)
	(self 'set-variable! 'effect-add effect-add)
	(self 'set-variable! 'effect-delete effect-delete)
	self
)

; create the initial plan
(define (make-minimal-plan initial goal)
	(let* ((S0 (operator 'new :action 'start :effect-add initial))
	       (S1 (operator 'new :action 'finish :precond goal))
	       (plan (plan-class 'new :steps (list (cons 0 S0) (cons 1 S1))
			:ordering (list (cons 0 1))
		        :subgoals (map (lambda(x) (cons 1 x)) goal)))) 
		plan
	)
)

(define (pop initial goal)
   (let ((plans (make-empty-queue)))
	(enqueue-at-front plans (list (make-minimal-plan initial goal)))
	(pop-plan plans)
   )
)

(define (pop-plan plans)
	(if (null? plans)
		(begin (newline) (display "no plan found") nil)
		(let* ((plan (remove-front plans))
		       (subgoals (plan 'get-variable 'subgoals))
		       (bindings (plan 'get-variable 'bindings)))

			(newline) (newline) (display "subgoals: ")
			(display (subst-bindings bindings subgoals))
			(print-plan plan)
			(if (null? subgoals)
				plan ; plan complete
				(pop-plan (refine plan plans))
			)
		)
	)
)

(define (refine plan plans)
   (let* ((subgoals (plan 'get-variable 'subgoals))
	  (steps (plan 'get-variable 'steps))
	  (Sneed (assoc (caar subgoals) steps))
	  (c (cdar subgoals)))
	(enqueue-at-end plans (try-existing-steps plan Sneed c))
	(enqueue-at-end plans (try-new-steps plan Sneed c))
   )
)


; refine plan by using existing steps
(define (try-existing-steps plan Sneed c)
    (let ((steps (plan 'get-variable 'steps))
	  (ordering (plan 'get-variable 'ordering))
	  (bindings (plan 'get-variable 'bindings))
	  (links (plan 'get-variable 'links))
	  (subgoals (plan 'get-variable 'subgoals))
	  (plans nil))

	(define (try-steps remaining-steps)
		(if (null? remaining-steps)
			plans	; return generated plans
			(let* ((Sadd (car remaining-steps))
			       (cadd ((cdr Sadd) 'get-variable 'effect-add))
			       (u (unify-effect c cadd bindings)))

				(if (and u (consistent (car Sadd) (car Sneed) 
						ordering))
				   (let* ((p (plan-class 'new
						:steps steps
						:ordering (add-ordering
							     (cons (car Sadd)
								   (car Sneed))
							      ordering)
						:bindings u
						:links (cons 
							(list (car Sadd) c
							      (car Sneed))
							 links)
						:subgoals (cdr subgoals) ))
					 (m (check-threats (list p)
						(p 'get-variable 'links)
						(p 'get-variable 'steps))) )
					(if m
					    (set! plans (append plans m))
					)
				    )
				)
				; try more plans
				(try-steps (cdr remaining-steps))
			)
		)
	)

	; try-existing-steps
	(try-steps steps)
   )
)

; try creating a new step
(define (try-new-steps plan Sneed c)
    (let ((steps (plan 'get-variable 'steps))
	  (ordering (plan 'get-variable 'ordering))
	  (bindings (plan 'get-variable 'bindings))
	  (links (plan 'get-variable 'links))
	  (subgoals (plan 'get-variable 'subgoals))
	  (plans nil))

	(define (try-op remaining-ops)
		(if (null? remaining-ops)
			plans	; return plans
			(let* ((Sadd (renameStepvars (car remaining-ops)))
			       (cadd (Sadd 'get-variable 'effect-add))
			       (u (unify-effect c cadd bindings)))
				(if u
				    (let* ((newnum (length steps))
					   (p (plan-class 'new
						:steps (cons (cons newnum Sadd)
							     steps)
						:ordering (add-ordering
							    (cons newnum
								  (car Sneed))
							     (add-ordering
							      (cons 0 newnum)
							      (add-ordering
								(cons newnum 1)
								ordering)))
						:bindings u
						:links (cons 
							(list newnum c
								(car Sneed))
							links)
						:subgoals (append 
							(map (lambda(x)
								(cons newnum x))
							   (Sadd 'get-variable
								'precond))
							(cdr subgoals) )))
					   (m (check-threats (list p) 
						(p 'get-variable 'links)
						(p 'get-variable 'steps))))
					(if m
					    (set! plans (append plans m))
					)
				      )
				)
				; try more operators
				(try-op (cdr remaining-ops))
			)
		)
	)

	; try operators
	(try-op operators)
   )
)
					


(define (unify-effect c effect bindings)
	(if (null? effect)
		+fail+
		(let ((u (unify c (car effect) bindings)))
			(if (equal? u +fail+)
				(unify-effect c (cdr effect) bindings)
				u
			)
		)
	)
)


(define (renameStepvars step)
	(let* ((vars (append 
			(variables-in (step 'get-variable 'action))
			(variables-in (step 'get-variable 'precond))
			(variables-in (step 'get-variable 'effect-add))
			(variables-in (step 'get-variable 'effect-delete))))
	       (bindings (map (lambda(var) 
				(make-binding var (new-variable var)))
			      vars))
	       (new-step (operator 'new :action (rename-variables 
						(step 'get-variable 'action)
						bindings)
				    :precond (rename-variables
						(step 'get-variable 'precond)
						bindings)
				    :effect-add (rename-variables
						(step 'get-variable 'effect-add)
						bindings)
				    :effect-delete (rename-variables
						(step 'get-variable 
							'effect-delete)
						bindings))))
		new-step
	)
)

; tests for possibility of interference between the any links and steps
; the list of plans.  It resolves any threats if possible
(define (check-threats plans links steps)
	(if (null? links)	; all links checked
		plans	
		; check the next link in all the plans
		(check-threats (check-link plans (car links) steps)
				(cdr links) steps)
	)
)

; check the link in all the plans against all the remaining steps
(define (check-link plans link steps)
	(if (null? steps) 	;all steps checked
		plans
		; check the next step against the link in all plans
		(check-link (check-plans plans link (car steps))
			     link (cdr steps))
	)
)

; check all plans against threat with link and step
(define (check-plans plans link step &optional (newplans nil))
	(if (null? plans)
		newplans	; all plans resolved
		(check-plans (cdr plans) link step 
			(append newplans 
				(check-if-threat (car plans) link step)))
	)
)

; return all resolved plans if threat with link and step 
; otherwise return list with original plan
(define (check-if-threat plan link step)
	(if (threat? plan step link)
		(resolve-threat plan step link)
		(list plan)
	)
)			



; attempt to resolve the threat of step to the link
(define (resolve-threat plan step link)
	(let* ((steps (plan 'get-variable 'steps))
	       (ordering (plan 'get-variable 'ordering))
	       (bindings (plan 'get-variable 'bindings))
	       (links (plan 'get-variable 'links))
	       (subgoals (plan 'get-variable 'subgoals))
	       (S1 (car link))
	       (S2 (caddr link)) 
	       (resolved nil))

(newline) (newline) (display "resolve threat of ") 
(display (subst-bindings bindings ((cdr step) 'get-variable 'action)))
(newline) (display "with link ") (display S1) (display " ") (display S2)

		(if (consistent (car step) S1 ordering)
			; resolve by demotion
			(set! resolved 			   
			  (list (plan-class 'new 
				   :steps steps
				   :ordering (add-ordering (cons (car step)
								 S1) ordering)
				   :bindings bindings
				   :links links
				   :subgoals subgoals )))
		)
		(if (consistent S2 (car step) ordering)
			; resolve by promotion
			(set! resolved (append resolved
			  (list (plan-class 'new 
				   :steps steps
				   :ordering (add-ordering (cons S2 (car step))
								 ordering)
				   :bindings bindings
				   :links links
				   :subgoals subgoals ))))
		)				
		resolved
		
	)
) 
	

(define (threat? plan step link)
   (let* ((S1 (car link))
	  (c (cadr link))
	  (S2 (caddr link))
	  (bindings (plan 'get-variable 'bindings))
	  (ordering (plan 'get-variable 'ordering))
	  (effect-del ((cdr step) 'get-variable 'effect-delete)) 
	  (clause (assoc (car c) effect-del)))
	(and (equal? (subst-bindings bindings c)
		     (subst-bindings bindings clause))
	     (can-be-between S1 (car step) S2 ordering)
	)
   )
)	

(define (can-be-between x y z ordering)
	(cond ((precedes y x ordering) #f)
	      ((precedes z y ordering) #f)
	      ((= x y) #f)
	      ((= y z) #f)
	      (else #t)
	)
)

(define (precedes x y ordering)
	(or (member? (cons x y) ordering)
	    (try-transitivity (keep (lambda(v)(= (car v) x)) ordering)
				y
				ordering)
	)
)

(define (try-transitivity x-ordering y ordering)
	(define (try x-list)
		(if (null? x-list)
			#f
		       (if (precedes (cdr (car x-list)) y ordering)
		     		#t
				(try (cdr x-list))
			)

		)
	)
	(try x-ordering)
)

(define (consistent a b ordering)
	(cond ((= a b) #f)
	      ((precedes b a ordering) #f)
	      (else #t)
	)
)

(define (add-ordering pair ordering)
	(if (member? pair ordering)
		ordering
		(cons pair ordering)
	)
)


(define (print-plan plan)
	(let* ((links (plan 'get-variable 'links))
	      (bindings (plan 'get-variable 'bindings))
	      (ordering (plan 'get-variable 'ordering))
	      (steps (plan 'get-variable 'steps))
	      (action (lambda(num) ((cdr (assoc num steps))
				     'get-variable 'action))) )
		(dotimes (i (length ordering))
			(let ((order (list-ref ordering i)))
				(newline)
				(display (subst-bindings bindings
					(action (car order))))
				(display " < ")
				(display (subst-bindings bindings 
					(action (cdr order))))
			)
		)
		(dotimes (i (length links))
			(let* ((link (list-ref links i))
			       (S1 (cdr (assoc (car link) steps)))
			       (S2 (cdr (assoc (caddr link) steps))))
			        (newline)
				(display (subst-bindings bindings
						(S1 'get-variable 'action)))
				(display " -> [")
				(display (subst-bindings bindings (cadr link)))
				(display "] ")
				(display (subst-bindings bindings
						(S2 'get-variable 'action)))
			)
		)
	)
)
		
