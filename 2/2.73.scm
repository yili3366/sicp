;; b
(define (deriv-sum exp var) 
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))

(define (deriv-product exp var) 
  (make-sum (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))

(define (install-deriv)
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  'done)

;; c
(define (deriv-exponentiation expr var)
  (let ((base (base expr))
        (exponent (exponent expr)))
    (make-product exponent
                  (make-product (make-exponentiation base (make-sum exponent -1))
                                (deriv base var)))))

(define (install-exponentiation-extension)
  (put 'deriv '** deriv-exponentiation)
  'done)

;; d
(put '** 'deriv deriv-exponentiation)
(put '+ 'deriv deriv-sum)
(put '* 'deriv deriv-product)
