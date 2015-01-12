;;TODO: implement for, do, until

;;; implemented as special forms in evaluator
;; (define (eval exp env)
;;   (cond ((self-evaluating? exp) exp)
;;         ((variable? exp) (lookup-variable-value exp env))
;;         ((quoted? exp) (text-of-quotation exp))
;;         ((assignment? exp) (eval-assignment exp env))
;;         ((definition? exp) (eval-definition exp env))
;;         ((if? exp) (eval-if exp env))
;;         ((lambda? exp)
;;          (make-procedure (lambda-parameters exp)
;;                          (lambda-body exp)
;;                          env))
;;         ((begin? exp)
;;          (eval-sequence (begin-actions exp) env))
;;         ((cond? exp) (eval (cond->if exp) env))
;;         ((application? exp)
;;          (apply (eval (operator exp) env)
;;                 (list-of-values (operands exp) env)))
;;         ((and? exp) (eval-and exp env))
;;         ((or? exp) (eval-or exp env))
;;         ((let? exp) (eval (let->combination exp) env))
;;         ((let*? exp) (eval (let*->nested-lets exp) env))
;;         (else
;;          (error "Unknown expression type - EVAL" exp))))


;; (define (and? exp)
;;   (tagged-list? exp 'and))
;; (define (eval-and exp env)
;;   (define (eval-and-operands operands)
;;     (cond ((null? operands) true)
;;           ((true? (eval (car operands) env))
;;            (eval-and-operands (cdr operands)))
;;           (else false)))
;;   (eval-and-operands (cdr exp)))

;; (define (or? exp)
;;   (tagged-list? exp 'or))
;; (define (eval-or exp env)
;;   (define (eval-or-operands operands)
;;     (cond ((null? operands) false)
;;           ((true? (eval (car operands) env))
;;            true)
;;           (else
;;            (eval-or-operands (cdr operands)))))
;;   (eval-or-operands (cdr exp)))

;;; as derived expressions

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        ((and? exp) (eval (and->if exp) env))
        ((or? exp) (eval (or->if exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((for? exp) (eval (for-combination exp) env))
        ((while? expr) (evaln (while->combination  expr) env))
        ((unbound? expr) (unbind-variable expr env))
        (else
         (error "Unknown expression type - EVAL" exp))))

(define (last-element lst)
  (if (null? (cdr lst))
      (car lst)
      (last-element (cdr lst))))

(define (tagged-list? exp sym)
  (if (pair? exp)
      (let ((last (last-element exp)))
        (eq? last sym))
      #f))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (car exp))
(define (if-consequent exp) (cadr exp))
(define (if-alternative exp)
  (if (= (length exp) 4)
      (caddr exp)
      'false))

(define (and? exp)
  (tagged-list? exp 'and))
(define (and-clauses exp) (cdr exp))
(define (expand-and-clauses clauses)
  (if (null? clauses)
      'true
      (make-if (car clauses)
               (expand-and-clauses (cdr clauses))
               'false)))
(define (and->if exp)
  (expand-and-clauses (and-clauses exp)))
                                        ;
(define (or? exp)
  (tagged-list? exp 'or))
(define (or-clauses exp) (cdr exp))
(define (expand-or-clauses clauses)
  (if (null? clauses)
      'false
      (make-if (car clauses)
               'true
               (expand-or-clauses (cdr clauses)))))
(define (or->if exp)
  (expand-or-clauses (or-clauses exp)))

;; (define (let? exp) (tagged-list? exp 'let))
;; (define (let-assignment exp) (cadr exp))
;; (define (let-body exp) (cddr exp))
;; (define (let-exp assignment)
;;   (if (null? assignment)
;;       '()
;;       (cons (cadr (car assignment))
;;             (let-exp (cdr assignment)))))
;; (define (let-var assignment)
;;   (if (null? assignment)
;;       '()
;;       (cons (car (car assignment))
;;             (let-var (cdr assignment)))))

;; (define (let->combination exp)
;;   (transform-let (let-assignment exp) (let-body exp)))
;; (define (transform-let assignment body)
;;   (cons (make-lambda (let-var assignment) body)
;;         (let-exp assignment)))

(define (named-let? expr) (and (let? expr) (symbol? (cadr expr))))
(define (named-let-func-name expr) (cadr expr))
(define (named-let-func-body expr) (cadddr expr))
(define (named-let-func-parameters expr) (map car (caddr expr)))
(define (named-let-func-inits expr) (map cadr (caddr expr)))
(define (named-let->func expr)
  (list 'define
        (cons (named-let-func-name expr) (named-let-func-parameters expr))
        (named-let-func-body expr)))

(define (let->combination expr)
  (if (named-let? expr)
      (sequence->exp
       (list (named-let->func expr)
             (cons (named-let-func-name expr) (named-let-func-inits expr))))
      (cons (make-lambda (let-vars expr)
                         (list (let-body expr)))
            (let-inits expr))))

(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-assignment exp) (cadr exp))
(define (let*-body exp) (cddr exp))

(define (let*->nested-lets exp)
  (transform-let* (let*-assignment exp) (let*-body exp)))
(define (transform-let* assignment body)
  (if (null? (cdr assignment))
      (cons 'let (cons assignment body))
      (list 'let (list (car assignment))
            (transform-let* (cdr assignment) body))))

(define (while? expr) (tagged-list? expr 'while))
(define (while-condition expr) (cadr expr))
(define (while-body expr) (caddr expr))
(define (while->combination expr)
  (sequence->exp
   (list (list 'define
               (list 'while-iter)
               (make-if (while-condition expr)
                        (sequence->exp (list (while-body expr)
                                             (list 'while-iter)))
                        'true))
         (list 'while-iter))))

(define (for-var exp) (cadr exp))
(define (for-init exp) (caddr exp))
(define (for-limit exp) (cadddr exp))
(define (for-body exp) (cddddr exp))

(define (for->combination exp)
  (list
   (make-lambda '()
                (list (list
                       'define
                       (list 'loop (for-var exp) 'n)
                       (list 'if
                             (list 'or
                                   (list '< (for-var exp) 'n)
                                   (list '= (for-var exp) 'n))
                             (cons 'begin
                                   (append (for-body exp)
                                           (list (list 'loop
                                                       (list '+ 1 (for-var exp))
                                                       'n))))))
                      (list 'loop (for-init exp) (for-limit exp))))))


(define (make-frame variables values)
  (cons
   'table
   (map cons variables values)))

(define (frame-pairs frame) (cdr frame-pairs))

(define (add-biding-to-frame! var val frame)
  (set-cdr! frame
            (cons (cons var val) (frame-pairs frame))))

;;   extend-environment
(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((ret (assoc var (frame-pairs (first-frame env)))))
          (if ret
              (cdr ret)
              (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variables -- SET!" var)
        (let ((ret (assoc var (frame-pairs (first-frame env)))))
          (if ret
              (set-cdr! ret val)
              (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
         (ret (assoc var (frame-pairs frame))))
    (if ret
        (set-cdr! ret val)
        (add-biding-to-frame! var val frame))))


;; this solution is based on exercise 4.11, that's to say i used different frame.
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too few arguments supplied" vars vals)
          (error "Too many arguments supplied" vars vals))))

;; look up a variable in a frame
(define (lookup-binding-in-frame var frame)
  (cond ((null? frame) (cons false '()))
        ((eq? (car (car frame)) var)
         (cons true (cdr (car frame))))
        (else (lookup-binding-in-frame var (cdr frame)))))

;; in frame, set var to val
(define (set-binding-in-frame var val frame)
  (cond ((null? frame) false)
        ((eq? (car (car frame)) var)
         (set-cdr! (car frame) val)
         true)
        (else (set-binding-in-frame var val (cdr frame)))))

(define (lookup-variable-value var env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var))
  (let ((result (lookup-binding-in-frame var (first-frame env))))
    (if (car result)
        (cdr result)
        (lookup-variable-value var (enclosing-environment env)))))

(define (set-variable-value! var val env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET" var)
      (if (set-binding-in-frame var val (first-frame env))
          true
          (set-variable-value! var val (enclosing-environment  env)))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (if (set-binding-in-frame var val frame)
        true
        (set-car! env (cons (cons var val) frame)))))

(define (unbound? expr) (tagged-list? expr 'unbound))
(define (unbind-variable expr env) (make-unbound (cadr expr) env))
(define (make-unbound variable env)
  (let ((vars (frame-variables (first-frame env)))
        (vals (frame-values (first-frame env))))
    (define (unbound vars vals new-vars new-vals)
      (cond ((null? vars)
             (error "variable is not in the environment -- MAKE-UNBOUND"

                    variable))
            ((eq? (car vars) variable)
             (set-car! env
                       (cons (append new-vars (cdr vars))
                             (append new-vals (cdr vals)))))
            (else (unbound (cdr vars) (cdr vals)
                           (cons (car vars) new-vars)
                           (cons (car vals) new-vals)))))
    (unbound vars vals '() '())))
