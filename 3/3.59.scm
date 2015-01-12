(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
;;a
(define (integrate-series coeffs)
  (stream-map / coeffs integers))
;;b
(define cosine-series
  (cons-stream 1 (integrate-series sine-series)))
(define sine-series
  (cons-stream 0 (scale-stream (integrate-series cosine-series) -1)))
