(define memoize
  (lambda (f)
    ((lambda (table)
       (lambda (x)
         ((lambda (previously-computed-result)
            (or previously-computed-result
                ((lambda (result)
                   (insert! x result table)
                   result)
                 (f x))))
          (lookup x table))))
     (make-table))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))
