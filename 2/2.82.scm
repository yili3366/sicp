;; put and get
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

(define (coerce-to target-type remaining-args result)
  (if (null? remaining-args)
      result
      (let* ((arg (car remaining-args))
             (original-type (type-tag arg)))
        (if (eq? original-type target-type)
            (coerce-to target-type
                       (cdr remaining-args)
                       (append result (list arg)))
            (let ((original->target (get-coercion (type-tag arg) target-type)))
              (if original->target
                  (coerce-to target-type
                             (cdr remaining-args)
                             (append result (list (original->target arg))))
                  #f))))))

(define (put-coercion source-type target-type proc)
  (put 'coercion (list source-type target-type) proc))

(define (get-coercion source-type target-type)
  (get 'coercion (list source-type target-type)))

(define (apply-generic-iter coercion-types)
  (if (null? coercion-types)
      (error "No method for these types, and could not coerce"
             (list op (map type-tag args)))
      (let ((coerced-args (coerce-to (car coercion-types) args '())))
        (if coerced-args
            (let ((proc (get op (map type-tag coerced-args))))
              (if proc
                  (apply proc (map contents coerced-args))
                  (apply-generic-iter (cdr coercion-types))))
            (apply-generic-iter (cdr coercion-types))))))

(define (uniquify l)
  (if (null? l)
      '()
      (let ((head (car l))
            (tail (cdr l)))
        (if (memq head tail)
            (uniquify tail)
            (cons head (uniquify tail))))))

;; apply-generic
(define (apply-generic op . args)
  (define (uniquify l)
    (if (null? l)
        '()
        (let ((head (car l))
              (tail (cdr l)))
          (if (memq head tail)
              (uniquify tail)
              (cons head (uniquify tail))))))
  (define (coerce-to target-type remaining-args result)
    (if (null? remaining-args)
        result
        (let* ((arg (car remaining-args))
               (original-type (type-tag arg)))
          (if (eq? original-type target-type)
              (coerce-to target-type
                         (cdr remaining-args)
                         (append result (list arg)))
              (let ((original->target (get-coercion (type-tag arg) target-type)))
                (if original->target
                    (coerce-to target-type
                               (cdr remaining-args)
                               (append result (list (original->target arg))))
                    #f))))))
  (define (apply-generic-iter coercion-types)
    (if (null? coercion-types)
        (error "No method for these types, and could not coerce"
               (list op (map type-tag args)))
        (let ((coerced-args (coerce-to (car coercion-types) args '())))
          (if coerced-args
              (let ((proc (get op (map type-tag coerced-args))))
                (if proc
                    (apply proc (map contents coerced-args))
                    (apply-generic-iter (cdr coercion-types))))
              (apply-generic-iter (cdr coercion-types))))))
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (let ((unique-types (uniquify type-tags)))
          (if (> (length unique-types) 1)
              (apply-generic-iter unique-types)
              (else (error "No method for this type"
                           (list op type-tags))))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (install-scheme-number-package)
  ;; internal procedures
  (define (tag x)
    (attach-tag 'scheme-number x))

  ;; interface to rest of the system
  (put 'make 'scheme-number
       (lambda (x)
         (tag x)))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y)
         (= x y)))
  (put '=zero? '(scheme-number)
     (lambda (value)
          (= value 0)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

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
  ;; interface to rest of the system
  (define (tag x)
    (attach-tag 'rational x))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'add '(rational rational)
        (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
        (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
        (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
        (lambda (x y) (tag (div-rat x y))))

  (put 'equ? '(rational rational)
       (lambda (x y)
         (and (= (numer x) (numer y))
              (= (denom x) (denom y)))))
  (put '=zero? '(rational)
       (lambda (r)
          (= 0 (numer r))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; polar
(define (install-polar-package)

  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))

  (define (make-from-mag-ang r a) (cons r a))

  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))

  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))

  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)

  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))

  'done)

;; (define (make-from-mag-ang r a)
;;   ((get 'make-from-mag-ang 'polar) r a))

;; rectangular
(define (install-rectangular-package)

  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))

  (define (make-from-real-imag x y) (cons x y))

  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))

  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)

  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))

  'done)

;; (define (make-from-real-imag x y)
;;   ((get 'make-from-real-imag 'rectangular) x y))

(define (install-complex-package)

  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))

  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; interal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))

  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag z)
    (attach-tag 'complex z))

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'complex
       (lambda (r a)
         (tag (make-from-mag-ang r a))))

  (put 'add '(complex complex)
       (lambda (z1 z2)
         (tag (add-complex z1 z2))))

  (put 'sub '(complex complex)
       (lambda (z1 z2)
         (tag (sub-complex z1 z2))))

  (put 'mul '(complex complex)
       (lambda (z1 z2)
         (tag (mul-complex z1 z2))))

  (put 'div '(complex complex)
       (lambda (z1 z2)
         (tag (div-complex z1 z2))))

  (put 'equ? '(complex complex)
       (lambda (x y)
         (and (= (real-part x) (real-part y))

              (= (imag-part x) (imag-part y)))))
  (put '=zero? '(complex)
       (lambda (c)
         (and (= 0 (real-part c))
              (= 0 (imag-part c)))))

  ;; equ? 的另一种实现，对比 magnitude 和 angle

  ;; (put 'equ? '(complex complex)
  ;;    (lambda (x y)
  ;;        (and (= (magnitude x) (magnitude x))
  ;;             (= (angle x) (angle y)))))

  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-polar-package)
(install-rectangular-package)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(define (install-coercion-package)
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
  (define (scheme-number->rational n)
    (make-rational (contents n) 1))
  (put-coercion 'scheme-number 'rational scheme-number->rational)
  (put-coercion 'scheme-number 'complex scheme-number->complex)
  'done)

(install-coercion-package)
