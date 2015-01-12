(define (square x) (* x x))
 
(define (even? n)
  (= (remainder n 2) 0))
 
(define (fast-expt b n)
  (define (fast-expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter (square b) (/ n 2) a))
          (else (fast-expt-iter b (- n 1) (* a b)))))
  (fast-expt-iter b n 1))
 
(define (variable? x) (symbol? x)) 
 
(define (same-variable? v1 v2) 
  (and (variable? v1) (variable? v2) (eq? v1 v2))) 
 
(define (sum? x) 
  (and (pair? x) (eq? (car x) '+))) 
 
(define (addend s) (cadr s)) 
 
(define (binary-expression? e)
  (null? (cdddr e)))

(define (augend s)
  (if (binary-expression? s)
      (second-term s)
      (cons '+ (all-but-first-term s))))

(define (second-term e)
  (caddr e))
 
(define (all-but-first-term e)
  (cddr e))

(define (product? x) 
  (and (pair? x) (eq? (car x) '*))) 
 
(define (multiplier p) (cadr p)) 
 
(define (multiplicand p)
  (if (binary-expression? p)
      (second-term p)
      (cons '* (all-but-first-term p))))

(define (reduce-expression e op)
  (if (binary-expression? e)
      (second-term e)
      (cons op (all-but-first-term e))))
 
(define (augend s) (reduce-expression s '+))
 
(define (=number? exp num) 
  (and (number? exp) (= exp num))) 
 
(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2) 
        ((=number? a2 0) a1) 
        ((and (number? a1) (number? a2)) (+ a1 a2)) 
        (else (list '+ a1 a2)))) 
 
(define (make-product m1 m2) 
  (cond ((or (=number? m1 0) (=number? m2 0)) 0) 
        ((=number? m1 1) m2) 
        ((=number? m2 1) m1) 
        ((and (number? m1) (number? m2)) (* m1 m2)) 
        (else (list '* m1 m2))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
 
(define (base e) (cadr e))
 
(define (exponent e) (caddr e))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (fast-expt b e))
        (else (list '** b e))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (let ((u (base exp))
               (n (exponent exp)))
           (make-product
            (make-product n
                          (make-exponentiation u
                                               (make-sum n -1)))
            (deriv u var))))
        (else
         (error "unknown expression type -- DERIV" exp))))
