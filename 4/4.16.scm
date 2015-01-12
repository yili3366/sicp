;; a, change look-up-variable-value
(define (lookup-variable-value var env)
  (define (env-lookup env)
    (define (scan vars vals)
      (cond ((null? vars) (env-lookup (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                 (error "variable is unassigned" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-lookup env))

;; b
(define (scan-out-defines body)
  (define (name-unassigned defines)
    (map (lambda (x) (list (definition-variable x) '*unassigned*)) defines))
  (define (set-values defines)
    (map (lambda (x)
           (list 'set! (definition-variable x) (definition-value x)))
         defines))
  (define (defines->let exprs defines not-defines)
    (cond ((null? exprs)
           (if (null? defines)
               body
               (list (list 'let (name-unassigned defines)
                           (make-begin (append (set-values defines)
                                               (reverse not-defines)))))))
          ((definition? (car exprs))
           (defines->let (cdr exprs) (cons (car exprs) defines) not-defines))
          (else (defines->let (cdr exprs) defines (cons (car exprs) not-defines)))))
  (defines->let body '() '()))

;; c
;; install scan-out-defines into make-procedure. otherwise, 
;; when we call procedure-body, procedure scan-out-defines will be called.

