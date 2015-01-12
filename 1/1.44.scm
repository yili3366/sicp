(define (compose f g)
  (lambda (x)
    (f (g x))))

;; recursion
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;; iteration 
(define (repeated f n) 
  (define (iter n result) 
    (if (= n 1) 
        result 
        (iter (- n 1) (compose f result)))) 
  (iter n identity)) 

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (n-fold-smooth f n) 
  ((repeated smooth n) f)) 
