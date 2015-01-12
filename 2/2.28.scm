(define nil (quote ()))
 
(define (fringe x)
  (cond ((null? x) nil)
        ((pair? x) (append (fringe (car x)) (fringe (cdr x))))
        (else (list x))))

;; (define (fringe x)
;;    (if (pair? x)
;;       (append (fringe (car x))
;;               (fringe (cdr x)))
;;       x))
