(define nil (quote ()))

(define (reverse items)
  (if (null? items)
      nil
      (append (reverse (cdr items)) (cons (car items) nil))))

(define (deep-reverse x)
  (cond ((null? x) nil)
        ((pair? x)
         (append (deep-reverse (cdr x)) (cons (deep-reverse (car x)) nil)))
        (else x)))

(define (deep-reverse x)
  (if (pair? x)
      (append (deep-reverse (cdr x))
              (list (deep-reverse (car x))))
      x))
