(define simplify 
	(lambda (lst1)
		(cond  
			; similar to myeval -- check is lst1 is a symbol or a number as base case. 
			((symbol? lst1)                           
            lst1)
            ((number? lst1)
              lst1)

            ; For each function  check what function it is and whether or not it is of length 3 or 2. 
            ; if it is of length three then it refers to a operation associated with multiplication, addition, subtraction, division and exponents. 
            ; if it is of length 2 then it is associated with with inc or dec. 
            ((and (equal? (second lst1) '+) (list? lst1) (= 3 (length lst1))) ; check if addition
            	(cond 
	            	((and (equal?  (simplify (first lst1)) 0) (equal? (simplify (third lst1)) 0)) ; check if both variable are 0, if so return 0. (0 + 0)
	            	(simplify 0))
	            	((equal? (simplify (first lst1)) 0) ; check if just the first variable is 0, if so return the third value. (0 + e)
	            		(simplify (third lst1)))
	            	((equal? (simplify (third lst1)) 0) ; check if the third variable is 0, if so return the first value. (e + 0)
	            		(simplify (first lst1)))
	            	;((and (number? (simplify (first lst1))) (number? (simplify (third lst1))))  
	            	;(+ (simplify (first lst1)) (simplify (third lst1))))
	            	(else
	            		(list (simplify (first lst1)) (car (cdr lst1)) (simplify (third lst1)))) ; if it is neither of the other conditions, save the format of the expression into a list. 
	            		)
	            	)

            ((and (equal? (second lst1) '-) (list? lst1) (= 3 (length lst1))) ; check if subtraction
                	(cond 
	            	((and (equal?  (simplify (first lst1)) 0) (equal? (simplify (third lst1)) 0))  
	            	(simplify 0))
	            	((equal? (simplify (first lst1)) (simplify (third lst1))) ; check if both variables equal each other, if so return 0 (e - e)
	            		0)
	            	;((equal? (simplify (first lst1)) 0) ; check if just the first variables is 0, if so return the third variables. (0 - e)
	            	;	(simplify (third lst1)))
	            	((equal? (simplify (third lst1)) 0) ; check if just the third variables is 0, if so return the first variables. (e - 0)
	            		(simplify (first lst1)))
	            	;((and (number? (simplify (first lst1))) (number? (simplify (third lst1))))  
	            	;(- (simplify (first lst1)) (simplify (third lst1))))
	            	(else
	            		(list (simplify (first lst1)) (car (cdr lst1)) (simplify (third lst1))) ; if it is neither of the other conditions, save the format of the expression into a list.
	            	)
	            )
            )

             ((and (equal? (second lst1) '*) (list? lst1) (= 3 (length lst1))) ; check if multiplication
                (cond 
	
	            	((and (equal? (simplify (first lst1)) 0) ) ; if the first variable is 0, return 0 (0 * e)
	            		0)
	            	((and (equal? (simplify (third lst1)) 0) ) ; if the third variable is 0, return 0 (e * 0)
	            		0)
	            	((equal? 1 (simplify (first lst1))) ; if the first variable is 1, return third variable (1 * e)
	            		(simplify (third lst1)))
	            	((equal? 1 (simplify (third lst1))); if the third variable is 1, return first variable ( e * 1)
	            		(simplify (first lst1)))
	            	;((and (number? (simplify (first lst1))) (number? (simplify (third lst1))))  
	            	;(* (simplify (first lst1)) (simplify (third lst1))))
	            	(else
	            		(list (simplify (car lst1)) (car (cdr lst1)) (simplify (car (cdr (cdr lst1)))))) ; if it is neither of the other conditions, save the format of the expression into a list.
	            	))

             ((and (equal? (second lst1) '/) (list? lst1) (= 3 (length lst1))) ; check if division
                (cond
	            	((equal? (simplify (third lst1)) 1) ; check if third variable is 1, return first variable (e / 1)
	            		(first lst1))
	            	;((and (number? (simplify (first lst1))) (number? (simplify (third lst1))))  
	            	;(/ (simplify (first lst1)) (simplify (third lst1))))
	            	(else
	            		(list (simplify (car lst1)) (car (cdr lst1)) (simplify (car (cdr (cdr lst1)))))) ; if it is neither of the other conditions, save the format of the expression into a list.
	            	)
         	)

             ((and (equal? (second lst1) '**) (list? lst1) (= 3 (length lst1))) ; check if exponent
             	(cond 
             		((equal? (simplify (third lst1)) 0) ; check if third variable is a 1; return 1 (e ** 0)
             			1)
             		((equal? (simplify (third lst1)) 1) ; check if third variable is a 1; return first variable (e ** 1)
             			(simplify (first lst1)))
             		((equal? 1 (simplify (first lst1))) ; check if first variable is a 1; return 1 ( 1 ** e)
             			1)
             		;((and (number? (simplify (first lst1))) (number? (simplify (third lst1))))  
	            	;(expt (simplify (first lst1)) (simplify (third lst1))))
             		(else
             			(list (simplify (car lst1)) (car (cdr lst1)) (simplify (car (cdr (cdr lst1)))))))) ; if it is neither of the other conditions, save the format of the expression into a list.
      

              ((and (equal? (car lst1) 'inc) (list? lst1) (= 2 (length lst1))) ; check if inc
              	(cond 
              		((number? (simplify (car (cdr lst1))))  ; if second value is a number ; return the number plus 1
              		(+ (simplify (car (cdr lst1))) 1))
              		(else
              			(list 'inc (simplify (second lst1)) )))) ; if it is neither of the other conditions, save the format of the expression into a list.

               ((and (equal? (car lst1) 'dec) (list? lst1) (= 2 (length lst1))) ; check if dec
              	(cond 
              		((number? (simplify (car (cdr lst1)))) ; if the second value is a number; return the number - 1
              		(- (simplify (car (cdr lst1))) 1))
              		(else
              			(list (simplify (second lst1)) '- '1)))) ; if it is neither of the other conditions, save the format of the expression into a list.

               (else
               	(error "simplify: invalid expression"))
        )

	)
)

