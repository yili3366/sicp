(define (make-interval a b) (cons a b))
(define (upper-bound interval) (max (car interval) (cdr interval)))
(define (lower-bound interval) (min (car interval) (cdr interval)))
(define (center i) (/ (+ (upper-bound i) (lower-bound i)) 2))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

;; Percent is between 0 and 100.0
(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100.0))))

(define (percent i)
  (* 100.0 (/ (width i) (center i))))
