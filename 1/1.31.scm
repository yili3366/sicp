;; recursion

;; (define (product term a next b)
;;   (if (> a b) 1
;;       (* (term a) (product term (next a) next b))))
;; iteration
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

;;(define (identity x) x)
;;(define (next x) (+ x 1))

;;(define (factorial n)
;;  (product identity 1 next n))

;; (define (pi-term n) 
;;   (if (even? n) 
;;       (/ (+ n 2) (+ n 1)) 
;;       (/ (+ n 1) (+ n 2)))) 

(define (factorial n)
  (product (lambda (x) x)
           1
           (lambda (i) (+ i 1))
           n))

(define (numer-term i)
  (cond ((= i 1)
         2)
        ((even? i)
         (+ i 2))
        (else
         (+ i 1))))

(define (denom-term i)
  (if (odd? i)
      (+ i 2)
      (+ i 1)))

(define (pi n)
  (* 4
     (exact->inexact
      (/ (product numer-term
                  1
                  (lambda (i) (+ i 1))
                  n)
         (product denom-term 
                  1
                  (lambda (i) (+ i 1))
                  n)))))
