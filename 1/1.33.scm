(define (inc x) (+ x 1))
(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; (define (filtered-accumulate combiner null-value term a next b filter) 
;;   (if (> a b)
;;       null-value 
;;       (if (filter a) 
;;           (combiner (term a)
;;                     (filtered-accumulate
;;                      combiner null-value term (next a) next b filter)) 
;;           (combiner null-value
;;                     (filtered-accumulate
;;                      combiner null-value term (next a) next b filter))))) 

(define (filtered-accumulate combine null-value term a next b filter)
  (define (iter i result)
    (cond ((> i b)
           result)
          ((if (filter i)
               (iter (next i) (combine (term i) result))
               (iter (next i) result)))))
  (iter a null-value))

(define (sum-of-prime-squares a b)
  (filtered-accumulate + 0 square a inc b prime?)) 


(define (gcd m n) 
  (cond ((< m n) (gcd n m)) 
        ((= n 0) m) 
        (else (gcd n (remainder m n))))) 
  
(define (relative-prime? m n) 
  (= (gcd m n) 1))

(define (product-of-relative-primes n) 
  (define (filter x) 
    (relative-prime? x n)) 
  (filtered-accumulate * 1 identity 1 inc n filter)) 
