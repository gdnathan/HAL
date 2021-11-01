(define (fib x)
    (cond ((eq? x 0) 0)
        ((eq? x 1) 1)
            (#t (+ (fib (- x 1)) (fib (- x 2))))))
