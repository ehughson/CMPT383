; my second  version of apply-env works by first splitting the environment into two lists: value list and variable list. 
; it then gets the index of the variable that matches v by using get-index to get the index of the variable. 
; original list: ((a 1) (b 2) (c 3) (b 4)) ; list1: (a b c b) list2: (1 2 3 4)
; it will then locate the value in the second list by using the index that was found in the first list. Then return that value unless the index is -1 it will return an error. 
(define apply-env
    (lambda (env v)
        (let ((n (get-index v (split1 env))))
        	(if (equal? n -1) 
        		(error "apply-env: empty environment") 
        			(list-ref (split2 env) n)))))

; make empty list by just returning the empty list. 
(define make-empty-env 
    (lambda ()
    '()))

; connect each list to make one list: it should make a list of pairs. 
(define extend-env 
        (lambda (v val lst)
            (cons (list v val) lst)))
       		

; Get index find the index by recursively looping through adding 1 until the variable is found that equals v. 
; it takes as input the v: value and the list of value that is made by split1. 
(define get-index
        (lambda (v lst)
                (if (null? lst)
                        -1
                        (if (equal? (car lst) v)
                                0
                                (if (equal? (get-index v (cdr lst)) -1) 
                                        -1
                                         (+ 1 (get-index v (cdr lst))))))))
 

 ; split1 splits the environment and makes a list of the variables. 
(define split1
    (lambda (lst)
        (cond ((null? lst) 
            '())
     	  (list (cons (car (car lst)) (split1 (cdr lst)))))))

; split2 splits the environemnt and makes a list of the values. 
(define split2
    (lambda (lst)
        (cond ((null? lst)
            '())
        	   (list (cons (second (car lst)) (split2 (cdr lst)))))))


(define test-env
    (extend-env 'a 1
        (extend-env 'b 2
            (extend-env 'c 3
                (extend-env 'b 4
                    (make-empty-env)))))
)
