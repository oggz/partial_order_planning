(ld "learn.lsp")

(define restaurant-problem
	(learning-problem 'new 
		:attributes '((alternate 1 0) (bar 1 0) (fri/sat 1 0)
			      (hungry 1 0) (patrons none some full)
			      (price cheap moderate expensive)
			      (raining 1 0)
			      (reservations 1 0)
			      (type french thai burger italian)
			      (waitestimate  <10 <30 <60 >60))
		:goals '((willwait 1 0))
		:examples '(((willwait . 1) (alternate . 1) 
			     (bar . 0) (fri/sat . 0) (hungry . 1)
			     (patrons . some) (price . expensive)
			     (raining . 0) (reservations . 1)
			     (type . french) (waitestimate . <10))
			    ((willwait . 0) (alternate . 1)
			     (bar . 0) (fri/sat . 0) (hungry . 1)
			     (patrons . full) (price . cheap)
			     (raining . 0) (reservations . 0)
			     (type . thai) (waitestimate . <60))
			    ((willwait . 1) (alternate . 0)
			     (bar . 1) (fri/sat . 0) (hungry . 0)
			     (patrons . some) (price . cheap)
			     (raining . 0) (reservations . 0)
			     (type . burger) (waitestimate . <10))
			    ((willwait . 1) (alternate . 1)
			     (bar . 0) (fri/sat . 1) (hungry . 1)
			     (patrons . full) (price . cheap)
			     (raining . 0) (reservations . 0)
			     (type . thai) (waitestimate . <30))
			    ((willwait . 0) (alternate . 1)
			     (bar . 0) (fri/sat . 1) (hungry . 0)
			     (patrons . full) (price . expensive)
			     (raining . 0) (reservations . 1)
			     (type . french) (waitestimate . >60))
			    ((willwait . 1) (alternate . 0)
			     (bar . 1) (fri/sat . 0) (hungry . 1)
			     (patrons . some) (price . moderate)
			     (raining . 1) (reservations . 1)
			     (type . italian) (waitestimate . <10))
			    ((willwait . 0) (alternate . 0)
			     (bar . 1) (fri/sat . 0) (hungry . 0)
			     (patrons . none) (price . cheap)
			     (raining . 1) (reservations . 0)
			     (type . burger) (waitestimate . <10))
			    ((willwait . 1) (alternate . 0)
			     (bar . 0) (fri/sat . 0) (hungry . 1)
			     (patrons . some) (price . moderate)
			     (raining . 1) (reservations . 1)
			     (type . thai) (waitestimate . <10))
			    ((willwait . 0) (alternate . 0)
			     (bar . 1) (fri/sat . 1) (hungry . 0)
			     (patrons . full) (price . cheap)
			     (raining . 1) (reservations . 0)
			     (type . burger) (waitestimate . >60))
			    ((willwait . 0) (alternate . 1)
			     (bar . 1) (fri/sat . 1) (hungry . 1)
			     (patrons . full) (price . expensive)
			     (raining . 0) (reservations . 1)
			     (type . italian) (waitestimate . <30))
			    ((willwait . 0) (alternate . 0)
			     (bar . 0) (fri/sat . 0) (hungry . 0)
			     (patrons . none) (price . cheap)
			     (raining . 0) (reservations . 0)
			     (type . thai) (waitestimate . <10))
			    ((willwait . 1) (alternate . 1)
			     (bar . 1) (fri/sat . 1) (hungry . 1)
			     (patrons . full) (price . cheap)
			     (raining . 0) (reservations . 0)
			     (type . burger) (waitestimate . <60)))
	)
)