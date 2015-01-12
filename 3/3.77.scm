(load "display-stream.scm")

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))


(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;;test

(stream-ref (solve (lambda (y) y) 1 0.0001) 10000)
