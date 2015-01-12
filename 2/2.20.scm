(define nil (quote ()))
 
(define (even? n)
  (= (remainder n 2) 0))
 
(define (same-parity first . rest)
  (let ((first-is-even (even? first)))
    (define (same-parity-as-first? n)
      (let ((n-is-even (even? n)))
        (or (and first-is-even n-is-even)
            (and (not first-is-even) (not n-is-even)))))
    (define (build-list rest)
      (cond ((null? rest) nil)
            ((same-parity-as-first? (car rest))
             (cons (car rest) (build-list (cdr rest))))
            (else (build-list (cdr rest)))))
    (cons first (build-list rest))))

(define (same-parity sample . others)
  (filter (if (even? sample)
              even?
              odd?)
          (cons sample others)))      ; sample should be included

