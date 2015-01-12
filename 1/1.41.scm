(define ((double f) x)
  (f (f x)))


(define (double f)
  (lambda (x)
    (f (f x))))
