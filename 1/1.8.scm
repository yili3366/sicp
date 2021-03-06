;; (define (good-enough? guess x)
;;   (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))

(define (cube x) (* x x x))

(define (good-enough? old-guess new-guess)
  (> 0.01
     (/ (abs (- new-guess old-guess))
        old-guess)))

;; (define (sqrt-iter guess x)
;;   (if (good-enough? guess x)
;;       guess
;;       (sqrt-iter (improve guess x)
;;                  x)))

(define (cube-root-iter guess x)
  (if (good-enough? guess (improve guess x))
      (improve guess x)
      (cube-root-iter (improve guess x)
                 x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess))
     3))

(define (average x y)
  (/ (+ x y) 2))

(define (cube-root x)
  (cube-root-iter 1.0 x))
