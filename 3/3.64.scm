(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))

(define (stream-limit s tolerance)
  (let ((s2 (stream-cdr s)))
    (if (< (abs (- (stream-car s)
                   (stream-car s2)))
           tolerance)
        (stream-car s2)
        (stream-limit s2 tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
