; The basic environment simulator code

(define-class obj
	(ivars  name	; name of object
		alive?  ; is the object alive?
		heading ; the heading of the object
		loc	; the location of the object
		bump	; has the object bumped into anything?
		container) ; some objects may contain others
)

(define-method (obj 'initialize  &key (name "?") (alive? nil)
			(heading '(1 0)) (loc '(1 1)) (bump nil) 
			(container nil) )
	(self 'set-variable! 'name name)
	(self 'set-variable! 'alive? alive?)
	(self 'set-variable! 'heading heading)
	(self 'set-variable! 'loc loc)
	(self 'set-variable! 'bump bump)
	(self 'set-variable! 'container container)
	self
)

(define-class environment
	(ivars 	agents 		; a list of the agents in the environment
		step		; the number of time steps simulated so far
		print		; whether or not to print
		state		; the state of the environment
	)
)


(define-method (environment 'initialize &key (print #t)) ; .
	(set! agents nil)
	(set! step 0)
	(self 'set-variable! 'print print)
	(set! state nil)
	self
)


(define-class agent-body ; the body of an agent
	(super-class obj)
)

(define-method (agent-body 'initialize) ; agent body is obj which is alive
	(super 'initialize :alive? #t :name nil)
	self
)



(define-class agent
	(ivars 	program		; fn: percept->action
		body		; the agent body
		score		; the performance measure
		percept		; the current percepts
		action		; the current action
		name		; the agent's name
	)
)

(define-method (agent 'initialize) ; initialize an agent
	(set! program 'nothing)
	(set! body (agent-body 'new))
	(set! score 0)
	(set! percept nil)
	(set! action nil)
	(set! name nil)
	self
)


; the basic environment simulator
(define (run-environment env &key (max-steps 200))

   (display-environment env)  ; print the environment

   ; increment the step
   (env 'set-variable! 'step (+ 1 (env 'get-variable 'step)))

   ; run until termination criteria met or reached max number of steps   
   (if (and (<= (env 'get-variable 'step) max-steps)
	    (not (env 'termination?)))
	(begin
	(let ((agents (env 'get-variable 'agents)))

	; for each agent in the environment do
		(dotimes (j (length agents))
			; get the jth agent
			(let ((a (list-ref agents j)))

				; set the agent percept
				(a 'set-variable! 'percept
					(env 'get-percept a))

				; set the agent action
				(a 'set-variable! 'action
					((a 'get-variable 'program)
						 (a 'get-variable 'percept)))
			)
		)
		; update the environment
		(env 'update-fn)

		; set the scores of all the agents
		(dotimes (j (length agents))
			(let ((a (list-ref agents j)))
				(a 'set-variable! 'score
					(env 'performance-measure a))
			)
		)
	)
	(run-environment env :max-steps max-steps))
   )
   'end-of-run
)

; Compare several agents on a set of N similar environment
(define (agent-trials env-fn agent-types &key (n 10))
	(let ((random-seed (get-time)))
		(map (lambda(agent-type) (agent-trial env-fn agent-type
						random-seed n))
			agent-types
		)
	)
	'end-of-trials
)

; Run N trials for a single agent
;Note: the random-seed allows reproduction of the same environments
;  for each time agent-trial in run with that seed
(define (agent-trial env-fn agent-type random-seed trials)
	(let ((total 0) (score 0))
		(dotimes (i trials)
			; so that all agents use same envs
			(set-random-seed! (+ i random-seed))
 			(let* ((env (apply (eval env-fn) 
					(list  ':aspec agent-type)))
				(agent (car (env 'get-variable 'agents))))
				
				(env 'set-variable! 'print #f)
				(run-environment env)
				(set! total (+ (agent 'get-variable 'score)
						total))
				(set! score (/ total trials))
			)
		)
		(display score)
		(display " average for ")
		(display agent-type)
		(newline)
	)
)


; print the environment
(define (display-environment env) 
   (if (env 'get-variable 'print) (begin
	(newline) (newline)
	(display "At Time Step ")
	(display (env 'get-variable 'step))
	(display ":") (newline)
	(if (> (env 'get-variable 'step) 0)
	(let ((agents (env 'get-variable 'agents)))
		(dotimes (i (length agents))
			(display "Agent [")
			(display (+ i 1))
			(display ",")
			(display ((list-ref agents i) 'get-variable 
				'score))
			(display "] ")
			(display " Perceives ")
			(display ((list-ref agents i) 'get-variable
				 'percept))
			(display " and does ")
			(display ((list-ref agents i) 'get-variable
				 'action))
			(newline)
		)
	))
	(env 'display-environment-snapshot)
   ))
)

(define-method (environment 'update-fn)
	(self 'execute-agent-actions))

(define-method (environment 'execute-agent-actions)
; each agent takes an action if legal
	(dotimes (i (length agents))
		(let* ((a (list-ref agents i))
			(act (a 'get-variable 'action)))
			(if (member? (op act) (self 'legal-actions))
				(apply (eval (op act)) 
					(append (list self 
						(a 'get-variable 'body))
						(args act)))
			)
		)
	)

)


(define (op exp) (if (list? exp) (car exp) exp))
(define (args exp) (if (list? exp) (cdr exp) exp))




