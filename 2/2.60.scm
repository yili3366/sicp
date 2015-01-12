(define (element-of-set? x set) 
  (cond ((null? set) false) 
        ((equal? x (car set)) true) 
        (else (element-of-set? x (cdr set))))) 

(define (adjoin-set x set) 
  (cons x set)) 

(define (union-set set1 set2) 
  (append set1 set2)) 

(define (intersection-set set1 set2)
  (define (iter set1 result)
    (if (or (null? set1) (null? set2))
        (reverse result)
        (let ((current-element (car set1))
              (remain-element (cdr set1)))
          (if (and (element-of-set? current-element set2)
                   (not (element-of-set? current-element result)))
              (iter remain-element
                    (cons current-element result))
              (iter remain-element result)))))
  (iter set1 '()))
