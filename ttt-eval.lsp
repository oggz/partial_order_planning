; Evaluation function for 2 players on 3x3 Tic-Tac-Toe board


(define (ttt-eval-fn state)
	(let* ((board (state 'get-variable 'board))
	      (players (state 'get-variable 'players))
	      (X2 (count-rows board 3 2 players))
	      (X1 (count-rows board 3 1 players))
	      (O2 (count-rows board 3 2 (reverse players)))
	      (O1 (count-rows board 3 1 (reverse players))))

		(list (+ (* 3 X2) X1 (- (* 3 O2)) (- O1))
		      (+ (* 3 O2) O1 (- (* 3 X1)) (- X1)))

	)
)

(define (ttt-pick-move state game)
	(minimax-cutoff-decision state game ttt-eval-fn 2))

(define (make-ttt-agent)
	(game-agent 'new :algorithm ttt-pick-move))

; count the number of rows that have k values of player1 and no other values
(define (count-rows board n k players)
	(let ((count 0)
	      (tries '((0 0 0 1) (0 0 1 0) (2 2 0 1) (2 2 1 0)
			(1 1 0 1) (1 1 1 0) (1 1 1 1) (1 1 1 -1))))
		(dotimes (i 8)
			(let* ((try (list-ref tries i))
	       			(x (car try)) (y (cadr try)) (dx (caddr try))
	       			(dy (cadddr try)))
				(if (check-k-free-row board x y n k 
					dx dy players)
					(set! count (+ 1 count))
				)
			)
		)
		count
	)
)

; check if k pieces of first player and none of second in the row
(define (check-k-free-row board x y n k dx dy players)
	(let ((player1 (car players)) (player2 (cadr players)))
	   (and (= k (+ (count-in-direction board x y n (- dx) (- dy) player1)
		     	(count-in-direction board x y n dx dy player1)
			(if (equal? (array-ref board x y) player1) -1 0)))
		(= 0 (+ (count-in-direction board x y n (- dx) (- dy) player2)
			(count-in-direction board x y n dx dy player2)))
	    )
	)
)


; Count number of player's pieces starting at (x,y) going in direction (dx dy)
(define (count-in-direction board x y n dx dy player)
  (if (and (< -1 x n) (< -1 y n))
	(if (equal? (array-ref board x y) player)
		(+ 1 (count-in-direction board (+ x dx) (+ y dy) n dx dy
					player))
		(count-in-direction board (+ x dx) (+ y dy) n dx dy player)
	)
	0
   )
)
