(define (for-each f items)
  (if (null? items)
      (newline)
      (let ()
        (f (car items))
        (for-each f (cdr items)))))
