(define nil (quote ()))
 
(define (square x) (* x x))
 
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((pair? tree) (cons (square-tree (car tree))
                            (square-tree (cdr tree))))
        (else (square tree))))

;; with map
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

