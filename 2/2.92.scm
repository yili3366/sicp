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
          (apply proc (map contents args))
          (error "No method for these types"
                 (list op type-tags))))))

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

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (negation x) (apply-generic 'negation x))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

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
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

; install-polynomial-package
(define (install-polynomial-package)
  ;; procedures 
  (define (make-poly variable term-list)
    (cons variable (remove-zeros term-list)))
  (define (remove-zeros L)
    (filter (lambda (term) (not (=zero? (coeff term)))) L))
  (define (filter predicate sequence)
    (cond ((null? sequence) '())
          ((predicate (car sequence))
           (cons (car sequence)
                 (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))
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
    (let ((v1 (variable p1))
          (v2 (variable p2)))
      (if (same-variable? v1 v2)
          (make-poly v1 (add-terms (term-list p1)
                                   (term-list p2)))
          (let ((order1 (var-order v1))
                (order2 (var-order v2)))
            (if (and order1 order2)
                (if (> order1 order2)
                    (add-poly 
                     p1 (make-poly v1 
                                   (list (make-term 0 (tag p2)))))
                    (add-poly 
                     p2 (make-poly v2 
                                   (list (make-term 0 (tag p1))))))
                (error "Don't know the order of the two variables -- ADD-POLY"
                       (list p1 p2)))))))
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
  (define (mul-poly p1 p2)
    (let ((v1 (variable p1))
          (v2 (variable p2)))
      (if (same-variable? v1 v2)
          (make-poly v1
                     (mul-terms (term-list p1)
                                (term-list p2)))
          (let ((order1 (var-order v1))
                (order2 (var-order v2)))
            (if (and order1 order2)
                (if (> order1 order2)
                    (mul-poly 
                     p1 (make-poly v1 
                                   (list (make-term 0 (tag p2)))))
                    (mul-poly 
                     p2 (make-poly v2 
                                   (list (make-term 0 (tag p1))))))
                (error "Don't know the order of the two variables -- ADD-POLY"
                       (list p1 p2)))))))
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
  (define (add-num-poly x p)
    (make-poly (variable p)
               (add-terms (list (make-term 0 x))
                          (term-list p))))
  (define (mul-num-poly x p)
    (make-poly (variable p)
               (map (lambda (term) (list (car term) (* x (cadr term))))
                    (term-list p))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polynomial x))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'add '(polynomial scheme-number)
       (lambda (p x) (tag (add-num-poly x p))))
  (put 'add '(scheme-number polynomial)
       (lambda (x p) (tag (add-num-poly x p))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'sub '(polynomial scheme-number)
       (lambda (p x) 
         (tag (add-num-poly 
               (negation (attach-tag 'scheme-number x))
               p))))
  (put 'sub '(scheme-number polynomial)
       (lambda (x p)
         (tag (add-num-poly x (neg-poly p)))))
  (put 'negation '(polynomial)
       (lambda (p) (tag (neg-poly p))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'mul '(polynomial scheme-number)
       (lambda (p x) (tag (mul-num-poly x p))))
  (put 'mul '(scheme-number polynomial)
       (lambda (x p) (tag (mul-num-poly x p))))
  (put '=zero? '(polynomial) 
       (lambda (p) (empty-termlist? (term-list p))))
  (put 'make 'polynomial
       (lambda (v t) (tag (make-poly v t))))
  'done)

;; outside constructor
(define (make-polynomial variable term-list)
  ((get 'make 'polynomial) variable term-list))

;; order of variables
(define (var-order v)
  (cond ((eq? v 'x) 0)
        ((eq? v 'y) 1)
        (else false)))

; tests
(install-scheme-number-package)
(install-polynomial-package)
