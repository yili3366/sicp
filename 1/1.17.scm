(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (multiply x y)
  (cond ((= y 0)
         0)
        ((even? y)
         (double (multiply x (halve y))))
        ((odd? y)
         (+ x (multiply x (- y 1))))))
