(load "constraint.scm")

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((diff (make-connector)))
    (adder y diff x)
    diff))

(define (c* x y)
  (let ((product (make-connector)))
    (multiplier x y product)
    product))

(define (c/ p q)
  (let ((r (make-connector)))
    (multiplier q r p)
    r))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))
