(define (make-sum a1 a2)
  (cond ((=number? a1 0)
         a2)
        ((=number? a2 0)
         a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else
         (list a1 '+ a2))))              ; 修改

(define (sum? x)
  (and (pair? x)
       (eq? (cadr x) '+)))                ; 修改

(define (addend s)
  (car s))                                ; 修改

(define (augend s)
  (caddr s))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0))
         0)
        ((=number? m1 1)
         m2)
        ((=number? m2 1)
         m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else
         (list m1 '* m2))))              ; 修改

(define (product? x)
  (and (pair? x)
       (eq? (cadr x) '*)))                ; 修改

(define (multiplier p)
  (car p))                                ; 修改

(define (multiplicand p)
  (caddr p))

;;; deriv
(define (deriv exp var)
  (cond ((number? exp)
         0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; number

(define (=number? exp num)
  (and (number? exp)
       (= exp num)))

;; variable

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? x num) (and (number? x) (= x num)))

;(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-sum a1 a2)
    (cond 
        ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (first-occurrence-of x exp)
    (define (iter answer remain)
        (cond
            ((null? remain) -1)
            ((eq? (car remain) x) answer)
            (else (iter (+ answer 1) (cdr remain)))))
    (iter 0 exp))

;(first-occurrence-of '+ '(1 2 3 4 5))

(define (is-op-exp? op e)
    (and (pair? e) (>= (first-occurrence-of op e) 0)))

(define (op-left op e) 
    (let ((occur (first-occurrence-of op e)))
        (if (= occur 1)
            (car e)
            (take e (first-occurrence-of op e)))))

(define (op-right op e) 
    (let ((occur (first-occurrence-of op e))
          (len (length e)))
        (if (= occur (- len 2))
            (list-ref e (- len 1))
            (drop e (+ 1 (first-occurrence-of op e))))))

(define (sum? e) (is-op-exp? '+ e))
(define (addend e) (op-left '+ e))
(define (augend e) (op-right '+ e))

(define (product? e) 
    ;since sum? test precedes product? test in deriv, 
    ;there's no need to do sum? test here again :)
    (is-op-exp? '* e))

(define (multiplier e) (op-left '* e))
(define (multiplicand e) (op-right '* e))

;(define (make-product m1 m2) (list '* m1 m2))

(define (make-product a1 a2)
    (cond
        ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list a1 '* a2))))

(define (deriv exp var)
    (cond 
        ((number? exp) 0)
        ((variable? exp) (if (eq? exp var) 1 0))
        ((sum? exp)
            (let ((u (addend exp))
                  (v (augend exp)))
                (make-sum
                    (deriv u var)
                    (deriv v var))))
        ((product? exp)
            (let ((u (multiplier exp))
                  (v (multiplicand exp)))
                (make-sum
                    (make-product u (deriv v var))
                    (make-product v (deriv u var)))))
        (else (error "unkown type of expression"))))

(deriv '(x + 3) 'x)

(deriv '(x + 3 + x * 2) 'x)

(deriv '(x * y + x * 3) 'x)

(deriv '(x * y * (x + 3)) 'x)
