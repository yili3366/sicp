(define (compose f g)
  (lambda (x)
    (f (g x))))

;; recursion
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;; iteration 
(define (repeat f n) 
  (define (iter n result) 
    (if (= n 1) 
        result 
        (iter (- n 1) (compose f result)))) 
  (iter n identity)) 
