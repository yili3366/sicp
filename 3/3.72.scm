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

(define (sum-square x) (+ (square (car x)) (square (cadr x)))) 
(define (squaresn s) 
  (define (stream-cadr s) (stream-car (stream-cdr s))) 
  (define (stream-caddr s) (stream-cadr (stream-cdr s))) 
  (let ((scar (stream-car s)) 
        (scadr (stream-cadr s)) 
        (scaddr (stream-caddr s))) 
    (if (= (sum-square scar) (sum-square scadr) (sum-square scaddr)) 
        (cons-stream (list (sum-square scar) scar scadr scaddr) 
                     (squaresn (stream-cdr (stream-cdr (stream-cdr s))))) 
        (squaresn (stream-cdr s))))) 
(define square-numbers  
  (squaresn (weighted-pairs integers integers sum-square))) 
