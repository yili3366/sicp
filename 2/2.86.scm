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

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

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

(define (find-highest-type l)
  (define (filter-type t f)
    (cond ((null? f) '())
          ((eq? (car f) t) (filter-type t (cdr f)))
          (else (cons (car f) (filter-type t (cdr f))))))
  (define (find-highest highest remaining-tower remaining-list)
    (cond ((null? remaining-list) highest)
          ((null? remaining-tower)
           (error "Cannot find highest type from non-tower types -- FIND-HIGHEST-TYPE"
                  remaining-list))
          (else (find-highest (car remaining-tower)
                              (cdr remaining-tower)
                              (filter-type (car remaining-tower) remaining-list)))))
  (find-highest #f tower-of-types l))

;; coercion
;;(define (integer->rational i) (make-rational i 1))
;;(define (rational->real r) (make-real (/ (numer r) (denom r))))
;;(define (real->complex r) (make-complex-from-real-imag r 0))

(define (in-tower? value)
  (and (pair? value) (memq (type-tag value) tower-of-types)))

(define tower-of-types '(integer rational real complex))

(define (is-lower? value type)
  (let ((type-and-higher (memq type tower-of-types)))
    (if (and type-and-higher
             (in-tower? value))
        (not (memq (type-tag value) type-and-higher))
        (error "Either value's type or type is not in tower-of-types"
               (list value type)))))

(define (apply-raise x types)
  (cond ((null? types)
         (error "Type not found in the tower-of-types"
                (list (type-tag x) tower-of-types)))
        ((eq? (type-tag x) (car types))
         (if (null? (cdr types))
             x
             (let ((raiser (get-coercion (type-tag x) (cadr types))))
               (if raiser
                   (raiser (contents x))
                   (error "No coercion procedure found for types"
                          (list (type-tag x) (cadr types)))))))
        (else (apply-raise x (cdr types)))))

(define (raise x)
  (apply-raise x tower-of-types))

(define (project x)
  (apply-raise x (reverse tower-of-types)))

(define (raise-to type value)
  (cond ((eq? type (type-tag value)) value)
        ((memq type tower-of-types) (raise-to type (raise value)))
        (else (error "Cannot raise to non-tower type -- RAISE-TO"
                     (list type tower-of-types)))))

(define (raise-all-to type values)
  (if (null? values)
      '()
      (cons (raise-to type (car values)) (raise-all-to type (cdr values)))))

(define (drop x)
  (let* ((dropped (project x))
         (raised (raise dropped)))
    (if (and (not (eq? (type-tag x) (type-tag dropped)))
             (equ? x raised))
        (drop dropped)
        x)))

(define (apply-generic op . args)
  (define (find-and-apply-op)
    (let* ((type-tags (map type-tag args))
           (proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (> (length args) 1)
              (let* ((highest-type (find-highest-type type-tags))
                     (mapped-args (raise-all-to highest-type args))
                     (mapped-types (map type-tag mapped-args))
                     (mapped-proc (get op mapped-types)))
                (if mapped-proc
                    (apply mapped-proc (map contents mapped-args))
                    (error
                     "No method for these types -- APPLY-GENERIC"
                     (list op type-tags))))))))
  (let ((result (find-and-apply-op)))
    (if (and (pair? result)
             (memq (type-tag result) tower-of-types))
        (drop result)
        result)))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (square-root x) (apply-generic 'sqrt x))
(define (square x) (apply-generic 'square x))
(define (arctan x y) (apply-generic 'arctan x y))
(define (cosine x) (apply-generic 'cosine x))
(define (sine x) (apply-generic 'sine x))

;;;
;;; Integer package
;;;
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (define (integer->rational i) (make-rational i 1))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (make-rational x y)))
  (put 'sqrt '(integer)
       (lambda (x) (make-real (sqrt x))))

  (put 'square '(integer)
       (lambda (x) (tag (* x x))))
  (put 'arctan '(integer integer)
       (lambda (x y) (make-real (atan x y))))
  (put 'cosine '(integer)
       (lambda (x) (make-real (cos x))))
  (put 'sine '(integer)
       (lambda (x) (make-real (sin x))))
  (put 'addd '(integer integer integer)
       (lambda (x y z) (tag (+ x y z))))

  (put 'equ? '(integer integer) =)
  (put '=zero? '(integer)
       (lambda (x) (= 0 x)))
  (put 'make 'integer
       (lambda (x) (if (integer? x)
                       (tag x)
                       (error "non-integer value" x))))
  (put-coercion 'integer 'rational integer->rational)
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (define (make-rat n d)
    (if (and (integer? n) (integer? d))
        (let ((g (gcd n d)))
          (cons (/ n g) (/ d g)))
        (error "non-integer numerator or denominator"
               (list n d))))
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

  (define (sqrt-rat x)
    (sqrt (/ (numer x) (denom x))))

  (define (square-rat x)
    (mul-rat x x))
  (define (arctan-rat x y)
    (atan (/ (numer x) (denom x))
          (/ (numer y) (denom y))))
  (define (cosine-rat x)
    (cos (/ (numer x) (denom x))))
  (define (sine-rat x)
    (sin (/ (numer x) (denom x))))

  (define (rational->integer r)
    (make-integer (round (/ (numer r) (denom r)))))
  ;; interface to rest of the system
  (define (tag x)
    (attach-tag 'rational x))
  (define (rational->real r) (make-real (/ (numer r) (denom r))))

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

  (put 'sqrt '(rational)
       (lambda (x) (make-real (sqrt-rat x))))
  (put 'square '(rational)
       (lambda (x) (tag (square-rat x))))
  (put 'arctan '(rational rational)
       (lambda (x y) (make-real (arctan-rat x y))))
  (put 'cosine '(rational)
       (lambda (x) (make-real (cosine-rat x))))
  (put 'sine '(rational)
       (lambda (x) (make-real (sine-rat x))))

  (put 'equ? '(rational rational)
       (lambda (x y)
         (and (= (numer x) (numer y))
              (= (denom x) (denom y)))))
  (put '=zero? '(rational)
       (lambda (r)
          (= 0 (numer r))))

  (put-coercion 'rational 'real rational->real)
  (put-coercion 'rational 'integer rational->integer)
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;;;
;;; Real package
;;;
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))

  (define (real->rational r)
    (make-rational (inexact->exact (numerator r))
                   (inexact->exact (denominator r))))
  (define (real->complex r)
    (make-complex-from-real-imag (tag r) (tag 0)))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))

  (put 'sqrt '(real)
       (lambda (x) (tag (sqrt x))))
  (put 'square '(real)
       (lambda (x) (tag (* x x))))
  (put 'arctan '(real real)
       (lambda (x y) (tag (atan x y))))
  (put 'cosine '(real)
       (lambda (x) (tag (cos x))))
  (put 'sine '(real)
       (lambda (x) (tag (sin x))))
  (put 'equ? '(real real) =)
  (put '=zero? '(real)
       (lambda (x) (= 0 x)))
  (put 'make 'real
       (lambda (x) (if (real? x)
                       (tag x)
                       (error "non-real value" x))))

  (put-coercion 'real 'complex real->complex)
  (put-coercion 'real 'rational real->rational)
  'done)

(define (make-real n)
  ((get 'make 'real) n))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;;; rectangular
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))

  (define (make-from-real-imag x y)
    (if (and (is-lower? x 'complex) (is-lower? y 'complex))
        (cons (drop x) (drop y))
        (error "non-real real or imaginary value" (list x y))))
  (define (magnitude z)
    (square-root (add (square (real-part z))
                     (square (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (if (and (is-lower? r 'complex) (is-lower? a 'complex))
        (cons (mul r (cosine a)) (mul r (sine a)))
        (error "non-real magnitude or angle" (list r a))))

  ;; (define (make-from-real-imag x y)
  ;;   (if (and (real? x) (real? y))
  ;;       (cons x y)
  ;;       (error "non-real real or imaginary value" (list x y))))

  ;; (define (magnitude z)
  ;;   (sqrt (+ (square (real-part z))
  ;;            (square (imag-part z)))))
  ;; (define (angle z)
  ;;   (atan (imag-part z) (real-part z)))

  ;; (define (make-from-mag-ang r a)
  ;;   (if (and (real? r) (real? a))
  ;;       (cons (* r (cos a)) (* r (sin a)))
  ;;       (error "non-real magnitude or angle" (list r a))))

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

;; polar
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))

  (define (make-from-mag-ang r a)
    (if (and (is-lower? r 'complex) (is-lower? a 'complex))
        (cons (drop r) (drop a))
        (error "non-real magnitude or angle" (list r a))))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y) 
    (if (and (is-lower? x 'complex) (is-lower? y 'complex))
        (cons (square-root (add (square x) (square y)))
              (arctan y x))
        (error "non-real real or imaginary value" (list x y))))

  ;; (define (make-from-mag-ang r a)
  ;;   (if (and (real? r) (real? a))
  ;;       (cons r a)
  ;;       (error "non-real magnitude or angle" (list r a))))

  ;; (define (real-part z)
  ;;   (* (magnitude z) (cos (angle z))))
  ;; (define (imag-part z)
  ;;   (* (magnitude z) (sin (angle z))))

  ;; (define (make-from-real-imag x y)
  ;;   (if (and (real? x) (real? y))
  ;;       (cons (sqrt (+ (square x) (square y)))
  ;;             (atan y x))
  ;;       (error "non-real real or imaginary value" (list x y))))

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

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))

  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; interal procedures
  (define (complex->real z)
    (make-real (real-part z)))

  ;; interface to rest of the system
  (define (tag z)
    (attach-tag 'complex z))

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
  (define (equ-complex? z1 z2)
    (and (equ? (real-part z1) (real-part z2))
         (equ? (imag-part z1) (imag-part z2))))
  (define (=zero-complex? x) (=zero? (magnitude x)))
  (define (addd-complex z1 z2 z3)
    (make-from-real-imag (addd (real-part z1)
                               (real-part z2)
                               (real-part z3))
                         (addd (imag-part z1)
                               (imag-part z2)
                               (imag-part z3))))

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

  (put-coercion 'complex 'real complex->real)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-polar-package)
(install-rectangular-package)
(install-integer-package)
(install-real-package)
(install-rational-package)
(install-complex-package)
