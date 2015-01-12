(define (square x) (* x x))

(define (sum-of-squares x y z)
  (cond          
   ((and (>= x z) (>= y z)) (+ (square x) (square y)))
   ((and (>= x y) (>= z y)) (+ (square x) (square z)))
   ((and (>= y x) (>= z x)) (+ (square y) (square z)))))

