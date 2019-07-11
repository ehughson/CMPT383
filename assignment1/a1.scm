; Question#1 Implement a function called (singleton? x) that returns #t if x is a list with exactly 1 element, and #f otherwise. 
(define singleton? 
	(lambda (x)
		(if (and (list? x) (equal? (cdr x) '())) #t #f)
	)	
)	
		   

; Question#2 Implement a function called (my-make-list n x) that returns a list containing n copies of x.

(define my-make-list
	(lambda (n x)
	 	(my-make-help '() n x)
	)
)

(define my-make-help 
	(lambda (lst n x)
		(if (equal? n 0) lst  (my-make-help (cons x lst) (- n 1) x))
	)
)


; Question#3. Implement a function called (all-same? lst) that returns #t if lst is empty, or if all the elements in it are equal to each other (using equal?). For example: 

(define all-same? 
	(lambda (lst)
		 (if (or (equal? (length lst) 0) (equal? (length lst) 1)) #t (all-same-help (car lst) (cdr lst)))
	)
)


(define all-same-help
	(lambda (x lst)
	(if (equal? (length lst) 0)	#t (if (equal? x (car lst)) (all-same-help x (cdr lst)) #f))		
	)
)

; question #4. Implement a function called (my-iota n) that returns a list containing the numbers from 0 to n-1. For example:
(define my-iota
	(lambda (n)
		(cond ((equal? n 0 ) '()) 
				(else 	
					(reversal (my-iota (- n 1)) (- n 1))
				)
		)
	)
)

(define reversal
	(lambda (lst x)
		(cond ((null? lst) (list x))
			(else 
				(cons (car lst) (reversal (cdr lst) x))
			)
		)
	)
)



; questio. #5. Implement a function called (my-length lst) that returns that returns the number of items in lst. For example:
(define my-length
		(lambda (lst)
			(cond ((null? lst) 
				0)
				(else
					(+ 1 (my-length (cdr lst)))
				)
			)
		)
)


;question #6. Implement a function called (nth lst i) that returns that returns the item at index location i in lst. The indexing is 0-based, so, the first element is at index location 0, the second element is at index location 1, and so on. For example:

(define nth 
		(lambda (lst i)
			(cond ((< i 0) (error "bad index") i)
				((null? lst) (error "bad index") lst)
					(else
						(if (equal? i 0) (car lst) (nth (cdr lst) (- i 1)))
					)
			)	
		)
)

;Question #7. Implement a function called (my-last lst) that returns the last element of lst. 
(define my-last
	(lambda (lst)
		 (cond ((null? lst)
		 	"my-last: empty list")
				(else (if (equal? (length lst) 1) (car lst) (my-last (cdr lst)))
				)
		)
	)
)

;Question #8. Implement a function called (middle lst) that returns a list that is the same as lst, but the first element and the last element have been removed
(define middle
	(lambda (lst)
		(cond ((equal? (my-length (cdr lst)) 1)
			 '()) 
			(else (cons (car (cdr lst)) (middle (cdr lst))))
		
		)
	)	
)

; Question #9. Implement a function called (my-filter pred lst) that returns a list containing just the elements of lst that satisfied the predicate function pred
(define my-filter
	(lambda (pred lst)
		(cond ((null? lst) 
			'())
				((equal? (pred (car lst)) #t)
					(cons (car lst) (my-filter pred (cdr lst))))
					(else 
						(my-filter pred (cdr lst))
					)
		)
	)
)



;Question #10. Implement a function called (my-append A B) that returns a list that has all the elements of A followed by all the elements of B.
(define my-append
	(lambda (A B)
		(cond ((null? A)
			B)
		 	(else (cons (car A) (my-append (cdr A) B))))
		
		)
	
)

; Question #11. Implement a function called (append-all lol) that returns a list that has all the lists of lol appended into one list. For example:

(define append-all 
	(lambda (lol)
		(cond ((not (list? lol))
				lol)
			((null? lol) 
				lol)
			((list? (car lol))
			(append (append-all (car lol)) (append-all (cdr lol))))
				(else (cons (car lol)
					(append-all (cdr lol)))
				)
		)
	)
)

;Question #12. Implement a function called (my-sort lst) that returns the numbers on lst in sorted order. For example:
; implemented selection sort to find min value and put in the front of the list. 
(define my-sort
	(lambda (lst)
		(cond ((null? lst)
			'())
				((equal? (car lst) (find-min lst (car lst)))
					(cons (car lst) (my-sort (cdr lst))))
					(else 
						(my-sort (my-append (cdr lst) (list (car lst)))))
				)	
		)
	)

(define find-min
	(lambda (lst n)
		(cond ((null? lst)
			n)
			(else
				(cond (( < (car lst) n)
					(find-min (cdr lst) (car lst)))
					(else
						(find-min (cdr lst) n))
				)
			)
		)	
	)
)


;Question #13. Implement a function called (all-bits n) that returns a list of 2n sub-lists, where each sub-list is a different pattern of n 0s and 1s. 
(define all-bits
	(lambda (n)
		(cond ((< n 0)
			'())
		((equal? n 1) 
			(make-bit-list '((0)) '((1)))) 
				(else
					(make-bit-list (add-zeros (all-bits (- n 1))) (add-ones (all-bits (- n 1)))))
				)
		)
	
)

(define add-ones
	(lambda (lst)
		(cond ((equal? lst '())
			'())
			(else (cons (cons 1 (car lst)) (add-ones (cdr lst)))
			)
		)
	)
)

(define add-zeros
	(lambda (lst)
		(cond ((equal? lst '())
			'())
				(else (cons (cons 0 (car lst)) (add-zeros (cdr lst)))
				)
		)
	)
)


 

(define make-bit-list
	(lambda (zeroL oneL)
		(cond ((equal? zeroL '())
			oneL)
				((equal? oneL '()) 
					'()) 
		 				(else (cons (car zeroL) (make-bit-list (cdr zeroL) oneL))
		 	)
		 )		
	)	
)


















