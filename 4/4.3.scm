(define eval-table (make-eq-hash-table))
(define get (lambda (key)
              (hash-table/get eval-table key #f)))
(define put (lambda (key proc)
              (hash-table/put! eval-table key proc)))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get (car exp))
         ((get (car exp)) exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(put 'quote
     (lambda (exp env)
       (text-of-quotation exp)))
(put 'set!
     (lambda (exp env)
       (eval-assignment exp env)))
(put 'define eval-definition)
(put 'if eval-if)
(put 'lambda
     (lambda (exp env)
       (make-procedure (lambda-parameters exp)
                       (lambda-body exp)
                       env)))
(put 'begin
     (lambda (exp env)
       (eval-sequence (begin-actions exp) env)))
(put 'cond
     (lambda (exp env)
       (eval (cond->if exp) env)))
