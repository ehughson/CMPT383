; check if the first value in the pair that is in the list matches v
; if it matches v then return the valye associated with it
; if it doesnt match pass, recursively, the rest of the list into apply-env. 
(define apply-env
    (lambda (env v)
        (cond ((null? env)
            (error "apply-env: empty environment"))
        ((equal?  v (first (car env))) (first (cdr (car env)))) 
            (else 
                (apply-env (cdr env) v)))))


(define make-empty-env 
    (lambda ()
    '()))



; make a list of the v and val and the add it to the rest of the lst that is being made. 
; it should become a list of pairs. 
(define extend-env 
        (lambda (v val lst)
            (cons (list v val) lst)))

; ((a 1) (b 2) (c 3) (b 4))

(define test-env
    (extend-env 'a 1
        (extend-env 'b 2
            (extend-env 'c 3
                (extend-env 'b 4
                    (make-empty-env)))))
)