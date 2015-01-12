(load "display-stream.scm")

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (weighted-pairs s t weight)
  (define (merge s1 s2)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          (else
           (let ((s1car (stream-car s1))
                 (s2car (stream-car s2)))
             (cond ((<= (weight s1car) (weight s2car))
                    (cons-stream s1car (merge (stream-cdr s1) s2)))
                   (else
                    (cons-stream s2car (merge s1 (stream-cdr s2)))))))))
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight))))

;; a
(define (weight-a pair)
  (+ (car pair) (cadr pair)))
(define sa (weighted-pairs integers integers weight-a))

;; b
(define (weight-b pair)
  (+ (* 2 (car pair)) (* 3 (cadr pair))
     (* 5 (car pair) (cadr pair))))
(define (divisible? x y) (= (remainder x y) 0))
(define stream235
  (stream-filter (lambda (x)
                   (and (not (divisible? x 2))
                        (not (divisible? x 3))
                        (not (divisible? x 5))))
                 integers))
(define sb (weighted-pairs stream235 stream235 weight-b))
