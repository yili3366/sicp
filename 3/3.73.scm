(load "display-stream.scm")

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC r c dt)
  (define (proc i v)
    (add-streams (scale-stream i r)
                 (integral (scale-stream i (/ 1 c)) v dt)))
  proc)

(define RC1 (RC 5 1 0.5))
