 (define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation - TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (put-coercion source-type target-type proc)
  (put 'coercion (list source-type target-type) proc))
(define (get-coercion source-type target-type)
  (get 'coercion (list source-type target-type)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (let ((to-drop (or (eq? op 'add) 
                             (eq? op 'sub)
                             (eq? op 'mul)
                             (eq? op 'div)
                             (eq? op 'sqrt-g)
                             (eq? op 'sine)
                             (eq? op 'cosine)
                             (eq? op 'atangent))))
            (if to-drop
                (drop (apply proc (map contents args)))
                (apply proc (map contents args))))
          (if (= (length args) 2)
              (let* ((a1 (car args))
                     (a2 (cadr args))
                     (level1 (tower-level a1))
                     (level2 (tower-level a2)))
                (cond ((< level1 level2)
                       (apply-generic op (raise-to level2 a1) a2))
                      ((< level2 level1)
                       (apply-generic op a1 (raise-to level1 a2)))
                      (else
                       (error "levels" level1 level2
                              "No method for these types"
                              (list op type-tags)))))
              (error "No method for these types"
                     (list op type-tags)))))))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

; level 0: scheme-number, integer
; level 1: rational
; level 2: scheme-number, real
; level 3: complex
(define (tower-level x) 
  (let ((typex (type-tag x)))
    (cond ((eq? typex 'rational) 1)
          ((eq? typex 'complex) 3)
          (else ; scheme-number
           (let ((y (contents x)))
             (if (exact-integer? y)
                 0
                 2))))))
(define (raise-to level x)
  (if (= level (tower-level x))
      x
      (raise-to level (raise x))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (negation x) (apply-generic 'negation x))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (sqrt-g x) (apply-generic 'sqrt-g x))
(define (square-g x) (apply-generic 'square-g x))
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (atangent x y) (apply-generic 'atangent x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))
(define (drop x)
  (let ((typex (type-tag x))
        (contentx (contents x)))
    (if (and (eq? typex 'scheme-number)
             (exact-integer? contentx))
        x
        (let ((proc (get 'project typex)))
          (if proc
              (let ((projected-x (project x)))
                (if (equ? (raise projected-x) x)
                    (drop projected-x)
                    x))
              x)))))

;; scheme number package
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'negation '(scheme-number)
       (lambda (x) (tag (- x))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'sqrt-g '(scheme-number)
       (lambda (x) (tag (sqrt x))))
  (put 'square-g '(scheme-number)
       (lambda (x) (tag (square x))))
  (put 'sine '(scheme-number) 
       (lambda (x) (tag (sin x))))
  (put 'cosine '(scheme-number) 
       (lambda (x) (tag (cos x))))
  (put 'atangent '(scheme-number scheme-number) 
       (lambda (x y) (tag (atan x y))))
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'raise '(scheme-number)
       (lambda (x)
         (if (exact-integer? x)
             (make-rational x 1)
             (make-complex-from-real-imag x 0))))
  (put 'project '(scheme-number) 
       (lambda (x)
         (define tolerance 0.001)
         (define (find-n-d nd)
           (let ((n (car nd))
                 (d (cadr nd)))
             (if (< (abs (/ (- n (round n)) n)) tolerance)
                 (list (inexact->exact (round n)) d)
                 (find-n-d (list (* 10 n) (* 10 d))))))
         (cond ((exact-integer? x) x)
               ((= x 0) (make-rational 0 1))
               (else
                (let ((nd (find-n-d (list x 1))))
                  (make-rational (car nd) (cadr nd)))))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'negation '(scheme-number)
       (lambda (x) (tag (- x))))
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; rational number package
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ-rat x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'negation '(rational)
       (lambda (x) (tag (make-rat (- (numer x))
                                  (denom x)))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'sqrt-g '(rational)
       (lambda (x) (div (numer x) (denom x))))
  (put 'square-g '(rational)
       (lambda (x) 
         (tag (make-rat (square (numer x))
                        (square (denom x))))))
  (put 'sine '(rational)
       (lambda (x) (sine (/ (numer x) (denom x)))))
  (put 'cosine '(rational)
       (lambda (x) (cosine (/ (numer x) (denom x)))))
  (put 'atangent '(rational rational)
       (lambda (x y) (atangent (/ (numer x) (denom x))
                               (/ (numer y) (denom y)))))
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  (put 'equ? '(rational rational) equ-rat)
  (put 'raise '(rational)
       (lambda (r)
         (make-scheme-number (exact->inexact (/ (numer r) (denom r))))))
  (put 'project '(rational)
       (lambda (r)
         (make-scheme-number
          (integer-round (numer r) (denom r)))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

; <complex-number-package>
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  (define (square-complex z)
    (make-from-mag-ang (square-q (magnitude z))
                       (mul 2 (angle z))))
  (define (equ-complex x y)
    (and (equ? (real-part x) (real-part y))
         (equ? (imag-part x) (imag-part y))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'negation '(complex)
       (lambda (z) (tag (negation z))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'square-g '(complex)
       (lambda (z) (tag (square-complex z))))
  (put 'equ? '(complex complex) equ-complex)
  (put '=zero? '(complex)
       (lambda (x) (equ? (magnitude x) 0)))
  (put 'project '(complex)
       (lambda (z) (raise-to (tower-level 1.0) (real-part z))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; rectangular package for complex numbers
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt-g (add (square-g (real-part z))
                 (square-g (imag-part z)))))
  (define (angle z)
    (atangent (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (mul r (cosine a)) (mul r (sine a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'negation '(rectangular)
       (lambda (z) 
         (tag (make-from-real-imag (- (real-part z))
                                   (- (imag-part z))))))
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; polar package for complex numbers
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt-g (add (square-g x) (square-g y)))
          (atangent y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'negation '(polar)
       (lambda (z) 
         (tag (make-from-mag-ang (- (magnitude z))
                                 (angle z)))))
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))
; </complex-number-package>

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-poly p1 p2) 
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2)))
  (define (neg-poly p) 
    (make-poly (variable p)
               (neg-terms (term-list p))))
  (define (neg-terms L)
    (if (empty-termlist? L)
        L
        (let ((t (first-term L)))
          (adjoin-term (make-term (order t) (negation (coeff t)))
                       (neg-terms (rest-terms L))))))
  (define (=zero-all-terms? L)
    (cond ((empty-termlist? L) #t)
          ((not (=zero? (coeff (first-term L)))) #f)
          (else (=zero-all-terms? (rest-terms L)))))
  (define (=zero-poly? p)
    (=zero-all-terms? (term-list p)))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'negation '(polynomial)
       (lambda (p) (tag (neg-poly p))))
  (put '=zero? '(polynomial) =zero-poly?)
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (install-coercion-package)
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
  ;; interface
  (put-coercion 'scheme-number 'complex scheme-number->complex)
  'done)

; tests
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-coercion-package)
(install-polynomial-package)
