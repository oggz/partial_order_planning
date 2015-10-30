; Deciding What Move to Make in a Game by Minimax 

; The minimax decision procedure returns the optimal move in the game
; using exhaustive generation of the entire game tree.  Implementation
; uses the fact that the evaluation and utility functions return a list of
; values from the point of view of each player, with the "current" player
; first. Hence, rather than using #'min, we always use #'max for the
; current player.  A successor value is passed up the tree using
; right-rotation.  This works for any number of players.

; The notation "a+s" means an (action . state) pair.

; Minimax

(define (minimax-decision state game)
  (car (the-biggest
        (lambda (a+s) (car (right-rotate (minimax-value (cdr a+s) game))))
        (game-successors state game))))

(define (minimax-value state game)
  (if (game 'game-over? state) 
      (terminal-values state)       
      (right-rotate
       	(the-biggest
        	(lambda (values) (car (right-rotate values)))
        	(mapcar (lambda (a+s) (minimax-value (cdr a+s) game))
                	(game-successors state game))))))

; move the last element to the front of the list
(define (right-rotate lst)
	(append (last-pair lst) (butlast lst)))

(define (the-biggest fn lst &optional (biggest nil) (best-val nil))
	(cond ((null? lst) biggest)
	      ((null? biggest) 
			(the-biggest fn (cdr lst) (car lst) (fn (car lst))))
	      ((> (fn (car lst)) best-val)
			(the-biggest fn (cdr lst) (car lst) (fn (car lst))))
	      (else (the-biggest fn (cdr lst) biggest best-val))
	)
)
	

; Minimax with Cutoff

(define (minimax-cutoff-decision state game eval-fn limit)
  (car (the-biggest
        (lambda (a+s) 
            (car (right-rotate 
                    (minimax-cutoff-value (cdr a+s) game eval-fn (- limit 1)))))
        (game-successors state game))))

(define (minimax-cutoff-value state game eval-fn limit)
  (cond ((game 'game-over? state) (terminal-values state))
	((>= 0 limit) (eval-fn state))
	(else (right-rotate
	    (the-biggest
	     (lambda (values) (car (right-rotate values)))
	     (mapcar (lambda (a+s) 
			 (minimax-cutoff-value (cdr a+s) game eval-fn 
					       (- limit 1)))
		     (game-successors state game)))))))

; return a list of (move . state) pairs that can be reached from this state
(define (game-successors state game)
  (mapcar (lambda (move) (cons move (game 'make-move state move)))
	  (game 'legal-moves state)))

; return the values of the state for each player
(define (terminal-values state)
  (mapcar (lambda (player) (cadr (assoc player (state 'get-variable 'scores))))
	  (state 'get-variable 'players)))

