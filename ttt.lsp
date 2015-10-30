; The Game of Tic-Tac-Toe
(ld "games.lsp")
(ld "ttt-eval.lsp")

; Generalized Tic-Tac-Toe, in which any number of players take turns
; placing marks on an NxN board, trying to get K marks in a row. 

(define-class ttt-game
	(super-class game)
	(ivars n	; the dimension of the board
	       k	; need to get k in a row
	)
)

(define-method (ttt-game 'initialize &key (n 3) (k 3) (players '(X O)))
	(super 'initialize)
	(self 'set-variable! 'n n)
	(self 'set-variable! 'k k)
	(let ((board (make-array n n))
	      (state (game-state 'new)))
		(dotimes (i n)
			(dotimes (j n)
				(array-set! board i j '-)
			)
		)
		(state 'set-variable! 'board board)
		(state 'set-variable! 'players players)
		(state 'set-variable! 'scores (map (lambda(player) 
							(list player 0))
						   players))
		(self 'set-variable! 'initial-state state)
	)
	self
)

(define (make-ttt-game &key (n 3) (k 3) (players '(X O)))
	(ttt-game 'new :n n :k k :players players)
)

; list all possible moves
(define-method (ttt-game 'legal-moves state)
  (let ((board (state 'get-variable 'board))
	 (moves nil))
    ; Iterate over all squares; make moves in empty ones.
     (dotimes (x n)
       (dotimes (y n)
	 (if (equal? (array-ref board x y) '-)
		(set! moves (cons (cons x y) moves))
    )))
     moves))

;return the new state that result from makin this move
(define-method (ttt-game 'make-move state move)
  ; copy board
  (let ((new-board (make-array n n))
        (board (state 'get-variable 'board))
	(players (state 'get-variable 'players))
	(new-state (game-state 'new)))
	(dotimes (i n)
	    (dotimes (j n)
		(array-set! new-board i j (array-ref board i j))
	    )
  	)
        ; make move on board
	(array-set! new-board (car move) (cdr move) (current-player state))
	; set up new state
	(new-state 'set-variable! 'board new-board)
	(new-state 'set-variable! 'players (append (cdr players) 
						(list (car players))))
	(new-state 'set-variable! 'scores (state 'get-variable 'scores))
	(new-state 'set-variable! 'previous-move move)
	new-state
  )
)

; check if the last player made a complete row, col. or diagonal of length k
; or it the board is full
(define-method (ttt-game 'game-over? state)
  (let* ((board (state 'get-variable 'board))
	 (players (state 'get-variable 'players))
	 (x (car (state 'get-variable 'previous-move)))
	 (y (cdr (state 'get-variable 'previous-move)))
	 (previous (previous-player state)))
    (cond ((and x y
		(or (check-k-in-a-row board x y n k +1  0 previous)
		    (check-k-in-a-row board x y n k  0 +1 previous)
		    (check-k-in-a-row board x y n k -1 +1 previous)
		    (check-k-in-a-row board x y n k +1 +1 previous)))
	      (state 'set-variable! 'scores
			(append (map (lambda(player) (list player -1))
					(butlast players))
				(list (list previous 1))))
	     'win)
	  ((board-full? board n)
	   'draw)
	  (else #f))))

;_________________________________________________________________________	  ; Auxiliary Functions

; Is the board full?
(define (board-full? board size)
   (let ((full #t))
	(dotimes (i size)
		(dotimes (j size)
			(if (equal? (array-ref board i j) '-)
				(set! full #f)
			)
		)
	)
	full
    )
)

; Does player have k in a row through (x y) in direction +/-dx +/-dy?
(define (check-k-in-a-row board x y n k dx dy player)
  (>= (+ (count-pieces-in-direction board x y n (- dx) (- dy) player) 
	 (count-pieces-in-direction board x y n dx dy player)
	 -1) ; because the piece at (x y) gets counted twice
      k))

; Count player's pieces starting at (x,y) going in direction (dx dy)
(define (count-pieces-in-direction board x y n dx dy player)
  (if (and (< -1 x n) (< -1 y n) (equal? (array-ref board x y) player))
      (+ 1 (count-pieces-in-direction board (+ x dx) (+ y dy)
				      n dx dy player))
    0))

;__________________________________________________________________________
; print the environment

(define (display-environment game-env)
   (let ((state (game-env 'get-variable 'state)))
      (if (state 'get-variable 'previous-move)
	(begin
		(newline)
		(display (previous-player state))
		(display " moved to ")
		(display (state 'get-variable 'previous-move))
		(newline)
	)
       )
      	(print-board (state 'get-variable 'board)
		     ((game-env 'get-variable 'game) 'get-variable 'n))
	(newline)
	(display (current-player state))
	(display " to move:") (newline)

   )
)

(define (print-board board n)
	; print header
	(newline)
	(print-repeated " " 4)
	(dotimes (x n)
		(display "|")
		(print-dashes 4)
	)
	(display "|") (newline)
	; print each row
	(dotimes (y1 n)
		(let ((y (- n y1 1)))
			(print-centered y 4)
			; print each location
			(dotimes (x n)
				(display "|")
				(print-centered (array-ref board x y) 4)
			)
			(display "|") (newline)
			; print dashed line
			(print-repeated " " 4)
			(dotimes (x n)
				(display "|")
				(print-dashes 4)
			)
			(display "|") (newline)
		  )
	)
	; print x-coord along bottom
	(print-repeated " " 4)
	(dotimes (x n)
		(display " ") (print-centered x 4)
	)	
)


(define (print-centered string n)
	(let ((blanks (- n (string-length (stringify string)))))
		(print-repeated " " (floor (/ blanks 2)))
		(display (stringify string))
		(print-repeated " " (ceiling (/ blanks 2)))
	)
)

(define (print-repeated string n)
	(dotimes (i n)
		(display string)))

(define (print-dashes n)
	(dotimes (i n)
		(display "-")))

(define (stringify exp)
	(cond ((number? exp) (number->string exp))
	      ((null? exp) "")
	      ((string? exp) exp)
	      ((symbol? exp) (symbol->string exp))
	      (else (string-append (get-name (car exp))
				(stringify (cdr exp))))
	)
)

