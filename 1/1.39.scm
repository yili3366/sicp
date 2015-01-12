;; recursion
(define (cont-frac N D k)

  (define (cf i)
    (if (= k i)
        (/ (N k) (D k))
        (/ (N i)
           (+ (D i) (cf (+ i 1))))))

  (cf 1))

;; iteration
(define (cont-frac N D k)

  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (N i)
                 (+ (D i) result)))))

  (iter (- k 1)
        (/ (N k) (D k))))

(define (tan-cf x k)
  (exact->inexact (cont-frac (lambda (i)
                 (if (= i 1)
                     x
                     (- (square x))))
               (lambda (i)
                 (- (* i 2) 1))
               k)))
