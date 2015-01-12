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

(define (triple x) (* x x x))
(define (sum-triple x) (+ (triple (car x)) (triple (cadr x))))

(define (Ramanujan s)
  (define (stream-cadr s) (stream-car (stream-cdr s)))
  (define (stream-cddr s) (stream-cdr (stream-cdr s)))
  (let ((scar (stream-car s))
        (scadr (stream-cadr s)))
    (if (= (sum-triple scar) (sum-triple scadr)) 
        (cons-stream (list (sum-triple scar) scar scadr)
                     (Ramanujan (stream-cddr s)))
        (Ramanujan (stream-cdr s)))))

(define Ramanujan-numbers
  (Ramanujan (weighted-pairs integers integers sum-triple)))

