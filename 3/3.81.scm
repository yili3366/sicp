(load "display-stream.scm")

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (random-numbers input-stream rand-init)
  (define (rand-update n) (+ 10 n))
  (let ((command (stream-car input-stream)))
    (cond ((eq? command 'generate)
           (let ((random-number (rand-update rand-init)))
             (cons-stream random-number
                          (random-numbers (stream-cdr input-stream) random-number))))
          ((and (pair? command) (eq? (car command) 'reset))
           (let ((random-number (rand-update (cadr command))))
             (cons-stream random-number
                          (random-numbers (stream-cdr input-stream) random-number))))
          (else
           '()))))



(define infinite-input
  (cons-stream 'generate infinite-input))
