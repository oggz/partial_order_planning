; Game-Playing
(ld "agents.lsp")
(ld "minimax.lsp")

; Definitions for all n-player turn-taking games.  The idea is that
; each particular game we want to deal with will define a subtype of
; the GAME structure, with methods for LEGAL-MOVES, MAKE-MOVE, and
; GAME-OVER?.
;
; In the description of a game, a player is an atomic name: X or O,
; or BLACK or WHITE.  The current state is kept as an instance of
; GAME-STATE (which we do not expect to create subtypes of).

; We use GAME->ENVIRONMENT to turn an instance of a game into an
; environment, which we can then run.  The function RUN-GAME
; provides a simple interface to do this. Corresponding to player X
; there is an agent with name X who has an agent program that
; chooses a move for X, given a game-state as the percept.

(define-class game
	(ivars	initial-state		; a state in the domain
		best			; best score for winning
		worst			; worst score for losing
	)
)

(define-method (game 'initialize)
	(set! best 1)
	(set! worst -1)
	self
)

(define-class game-state
	(ivars  board		; the current state of the game board
		players		; list of the players
		scores		; a list of scores for each player
		terminal?	; Does this state end the game?
		previous-move	; The move just made to get to this position
	)
)

(define-method (game-state 'initialize)
	(set! players '(X O))
	(set! scores '((X 0) (O 0)))
	(set! terminal? #f)
	(set! previous-move nil)
	self
)


; Generic Functions for Games

; return a list of legal moves
(define-method (game 'legal-moves state)
)
; return the new state theat result from making this move
(define-method (game 'make-move state move)
)
; Is the game finished?
(define-method (game 'game-over? state)
)

;______________________________________________________________________________
; Game-playing environments

(define-class game-environment
	(super-class environment)
	(ivars game)
)

(define-method (game-environment 'initialize game agents)
	(super 'initialize)
	(self 'set-variable! 'game game)
	(set! state (game 'get-variable 'initial-state))
	(self 'set-variable! 'agents  
		(mapcar (lambda(agent name)
				(agent 'set-variable! 'name name)
				((agent 'get-variable 'body) 'set-variable!
					'name name)
				(agent 'set-variable! 'program
					(lambda(state)
						(if (equal? agent 
							(current-agent self))
						   ((agent 'get-variable
							'algorithm)
							state game)
						   'nothing
						)
					)
				)
				agent)
			agents 
			(state 'get-variable 'players)))  
	self
)

; convert a game into an environment
(define (game->environment game agents)
  (game-environment 'new game agents))

; Build an environment around this game, and run it
(define (run-game game agents)
  (run-environment (game->environment game agents)))

;_____________________________________________________________________________
; Implementation of Generic Functions for Game Environments

(define-method (game-environment 'update-fn)
  (let ((action ((current-agent self) 'get-variable 'action)))
    ;; Only allow legal moves
    (if (legal-move? action state game)
      (let ((new-state (game 'make-move state action)))
	;; Copy the scores into the agent structures
	(assign-agent-scores state self)
	;; Update the environment
	(set! state new-state)))))

(define-method (game-environment 'performance-measure agent)
  (cadr (assoc (agent 'get-variable 'name) (state 'get-variable 'scores))))

; The agent can perceive the whole state
(define-method (game-environment 'get-percept agent)
  state)

(define-method (game-environment 'termination?)
	(let ((done (game 'game-over? state)))

    		(assign-agent-scores state self)
    		done))


(define (assign-agent-scores state env) 
  (map (lambda (player)
	    ((agent-with-name player env) 'set-variable! 'score 
		(cadr (assoc player (state 'get-variable 'scores)))))
	(state 'get-variable 'players)
   )
)



;______________________________________________________________________________
; Game playing agents

(define-class game-agent
	(super-class agent)
	(ivars algorithm)
)

(define-method (game-agent 'initialize &key (algorithm minimax-decision))
	(super 'initialize)
	(self 'set-variable! 'algorithm algorithm)
	self
)

(define (make-random-game-agent)
	(game-agent 'new :algorithm pick-random-move))

(define (pick-random-move state game)
	(random-element (game 'legal-moves state)) 
)

(define (random-element lst)
	(list-ref lst (random (length lst))))



;______________________________________________________________________________
; Auxiliary functions

(define (legal-move? move state game)
  (member? move (game 'legal-moves state) ))

(define (current-player game-state)
  (car (game-state 'get-variable 'players)))

(define (previous-player game-state)
  (last (game-state 'get-variable 'players)))

(define (game-players game)
  ((game 'get-variable 'initial-state) 'get-variable 'players))

(define (current-agent env)
  (agent-with-name (current-player (env 'get-variable 'state)) env))

(define (agent-with-name name env)
	(define (find-name name agents)
		(cond ((null? agents) nil)
		      ((equal? name ((car agents) 'get-variable 'name))
				(car agents))
		      (else (find-name name (cdr agents)))
		)
	)
	(let ((agents (env 'get-variable 'agents)))
		(find-name name agents)))



