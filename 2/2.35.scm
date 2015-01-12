;; (define (count-leaves x)
;;   (cond ((null? x) 0)
;;         ((not (pair? x)) 1)
;;         (else (+ (count-leaves (car x))
;;                  (count-leaves (cdr x))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
 
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (sub-tree)
                     (if (pair? sub-tree)
                         (count-leaves sub-tree)
                         1))
                   t)))
