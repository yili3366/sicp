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

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

;; test 
;;(add-streams (mul-series cosine-series cosine-series) (mul-series sine-se\ries sine-series))

