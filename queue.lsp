(define-class q
	(ivars key
		last
		elements
	)
)

(define-method (q 'initialize)
	(set! key nil)
	(set! last nil)
	(set! elements nil)
	self
)

(define (make-empty-queue)
	(q 'new)
)

(define (empty-queue? q)
	(null? (q 'get-variable 'elements))
)

(define (queue-front q)
	(list-ref (q 'get-variable 'elements) 0)
)

(define (remove-front q)
	    (let ((front (car (q 'get-variable 'elements))))
		(q 'set-variable! 'elements (cdr (q 'get-variable 'elements)))
		front
	     )
)

; Three enqueueing functions

(define (enqueue-at-front q items)
	(cond ((null? items) nil)
	      ((empty-queue? q) (q 'set-variable! 'last (last-pair items)))
	)
	(q 'set-variable! 'elements (append items (q 'get-variable 'elements)))
	q
)

(define (enqueue-at-end q items)
	(cond ((null? items) nil)
	      ((or (null? (q 'get-variable 'elements))
		   (null? (q 'get-variable 'last)))
			(q 'set-variable! 'last (last-pair items))
			(q 'set-variable! 'elements
				(append (q 'get-variable 'elements) items)))
	      (else	(set-cdr! (q 'get-variable 'last) items)	
			(q 'set-variable! 'last (last-pair items)))
	)
	q
)
		

(define (enqueue-by-priority q items key)
	(q 'set-variable! 'key key)
	(dotimes (i (length items))
			(priority-insert 	q 
			     			(list-ref items i)
			     			key)
	)
)

(define (priority-insert q item key)
	(let ((insert-after (find-insert-after (q 'get-variable 'elements)
						item key)))
		(if (null? insert-after)
			(q 'set-variable! 'elements 
				(cons item (q 'get-variable 'elements)))
			(set-cdr! insert-after 
				(cons item (cdr insert-after)))
		)
	)
)

(define (find-insert-after lst item key &optional (prev nil))
	(cond ((null? lst) prev)
	      ((>= (key (car lst)) (key item)) prev)
	      (else (find-insert-after (cdr lst) item key lst))
	)
)

