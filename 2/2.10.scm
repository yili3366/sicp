(define (make-interval a b) (cons a b))
(define (upper-bound interval) (max (car interval) (cdr interval)))
(define (lower-bound interval) (min (car interval) (cdr interval)))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (>= 0 (* (lower-bound y) (upper-bound y)))
      (error "Division error (interval spans 0)" y)
      (mul-interval x
                    (make-interval (/ 1. (upper-bound y))
                                   (/ 1. (lower-bound y))))))

(define (print-interval name i) 
  (newline) 
  (display name) 
  (display ": [") 
  (display (lower-bound i)) 
  (display ",") 
  (display (upper-bound i)) 
  (display "]")) 
  
