(define (entry tree) (car tree))
 
(define (left-branch tree) (cadr tree))
 
(define (right-branch tree) (caddr tree))
 
(define (make-tree entry left right)
  (list entry left right))
 
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))
 
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
 
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))
 
(define (union-set set1 set2)
  (define (union-set-list list1 list2)
    (cond ((null? list1) list2)
          ((null? list2) list1)
          (else (let ((x1 (car list1))
                      (x2 (car list2)))
                  (cond ((= x1 x2)
                         (cons x1 (union-set-list (cdr list1) (cdr list2))))
                        ((< x1 x2)
                         (cons x1 (union-set-list (cdr list1) list2)))
                        (else
                         (cons x2 (union-set-list list1 (cdr list2)))))))))
  (list->tree (union-set-list (tree->list set1)
                              (tree->list set2))))
 
(define (intersection-set set1 set2)
  (define (intersection-set-list list1 list2) 
    (if (or (null? list1) (null? list2)) 
        '()    
        (let ((x1 (car list1)) (x2 (car list2))) 
          (cond ((= x1 x2) 
                 (cons x1 
                       (intersection-set-list (cdr list1) 
                                         (cdr list2)))) 
                ((< x1 x2) 
                 (intersection-set-list (cdr list1) list2)) 
                ((< x2 x1) 
                 (intersection-set-list list1 (cdr list2))))))) 
  (list->tree (intersection-set-list (tree->list set1)
                                     (tree->list set2))))
