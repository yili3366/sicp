(define (set-to-wow! x)
  (set-car! (car x) 'wow!)
  x)

(define x (list 'a 'b))
(define z1 (cons x x))

(define z2 (cons (list 'a 'b) (list 'a 'b)))
