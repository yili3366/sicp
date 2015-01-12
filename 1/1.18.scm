(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (multiply a b)
  (multiply-iter a b 0))

(define (multiply-iter a b product)
  (cond ((= b 0)
         product)
        ((even? b)
         (multiply-iter (double a)
                        (halve b)
                        product))
        ((odd? b)
         (multiply-iter a
                        (- b 1)
                        (+ a product)))))
