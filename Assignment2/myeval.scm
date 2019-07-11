(load "env1.scm")


(define myeval 
    (lambda (lst1 lst2)
        (cond
            ; Two base cases to check if the value being sent in is either a list or a number. If it is a 
            ; list then return to apply-env to get the value associated with the co-efficient. 
            ((symbol? lst1)                           
            (apply-env lst2 lst1))
            ((number? lst1)
              lst1)
            ; in each condition -- pass recursively the first value and the second value to myeval until base case is reached. 
            ; Also send the list two  (the environment) to get the value assocaited with the co efficient. 
            ((and (equal? (second lst1) '+) (list? lst1) (= 3 (length lst1))) ; addition condition
                (+ (myeval (car lst1) lst2) (myeval (car (cdr (cdr lst1))) lst2)))
            
            ((and (equal? (second lst1) '-) (list? lst1) (= 3 (length lst1))) ; subtraction condition 
                (- (myeval (car lst1) lst2) (myeval (car (cdr (cdr lst1))) lst2))) 
            
             ((and (equal? (second lst1) '*) (list? lst1) (= 3 (length lst1))) ; multiplication condition 
                (* (myeval (car lst1) lst2) (myeval (car (cdr (cdr lst1))) lst2)))
             
             ((and (equal? (second lst1) '/) (list? lst1) (= 3 (length lst1))) ; division condition
                ((equal? (car (cdr (cdr lst))) 0) (error "0 occurred in the denominator"))
                (/ (myeval (car lst1) lst2) (myeval (car (cdr (cdr lst1))) lst2)))
            
             ((and (equal? (second lst1) '**) (list? lst1) (= 3 (length lst1))) ; exponent condition. 
                (expt (myeval (car lst1) lst2) (myeval (car (cdr (cdr lst1))) lst2))) 
             
             ((and (equal? (car lst1) 'inc) (list? lst1) (= 2 (length lst1))) ; inc condition 
               (+ (myeval (car (cdr lst1)) lst2) 1))
             
             ((and (equal? (car lst1) 'dec) (list? lst1) (= 2 (length lst1))) ; dec condition. 
               (+ (myeval (car (cdr lst1)) lst2) 1))
             (else
                (error))
         )
    )
)
            


(define env1
    (extend-env 'x -1
        (extend-env 'y 4
            (extend-env 'x 1
                (make-empty-env))))
)

(define env2
    (extend-env 'm -1
        (extend-env 'a 4
            (make-empty-env)))
)

(define env3
    (extend-env 'q -1
        (extend-env 'r 4
            (make-empty-env)))
)



