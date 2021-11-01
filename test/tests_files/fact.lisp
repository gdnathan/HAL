(define (fact x)
    (cond ((eq? x 1) 1)
        (#t (* x (fact (- x 1))))))
