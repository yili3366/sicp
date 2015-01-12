(load "3.60.scm")

(define (invert-unit-series s)
  (define X
    (cons-stream 1 (mul-stream -1 (mul-series (stream-cdr s) X))))
  X)

(define inverse-exp-series (invert-unit-series exp-series))
