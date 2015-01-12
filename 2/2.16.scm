(define (make-interval a b) (cons a b))
 
(define (lower-bound i) (car i))
 
(define (upper-bound i) (cdr i))
 
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
 
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
 
(define a (make-interval 2 4))
 
(define b (make-interval -2 0))
 
(define c (make-interval 3 8))
 
(define x (mul-interval a
                        (add-interval b c)))
 
(define y (add-interval (mul-interval a b)
                        (mul-interval a c)))
 
(lower-bound x)
