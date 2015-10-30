(define operators (list (operator 'new :action '(go $x)
				:precond '((at $y))
				:effect-add '((at $x))
				:effect-delete '((at $y)) )
			(operator 'new :action '(buy $x)
				:precond '((sells $y $x) (at $y))
				:effect-add '((have $x)) )
		  )
)

(define init '((at home) (sells sm bananas) (sells sm milk) (sells hws drill)))

(define goal '((have bananas) (have milk) (have drill) (at home)))
