(define operators (list (operator 'new :action '(move-to-block $new)
				:precond '((at $old) (clear $new))
				:effect-add '((at $new))
				:effect-delete '((at $old) (clear $new)) )
			(operator 'new :action '(move-to-table $x)
				:precond '((clear $x))
				:effect-add '((on-table $x)) )
		  )
)

(define init '((on-table B) (clear B) (on-table A) (on-block C A) (clear C)))

(define goal '((on-table C) (on-block B C) (on-block A B)))
