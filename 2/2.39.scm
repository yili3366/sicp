(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) 
              nil sequence))
 
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x))
             nil sequence))

(define (fold-left op initial sequence) 
  (define (iter result rest) 
    (if (null? rest) 
        result 
        (iter (op result (car rest)) 
              (cdr rest)))) 
  (iter initial sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
 
(define (fold-right op initial sequence)
  (accumulate op initial sequence))

;; To guarantee that fold-right and fold-left will produce the same values for any sequence, op must be commutative.
