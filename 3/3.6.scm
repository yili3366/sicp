(define (rand-update x)
  (let ((a (expt 2 32))
        (c 1103515245)
        (m 12345))
    (modulo (+ (* a x) c) m)))

(define random-init 137)

(define rand 
  (let ((x random-init))
    (define (dispatch m)
      (cond ((eq? m 'generate)
             (begin (set! x (rand-update x))
                    x))
            ((eq? m 'reset) 
             (lambda (new-x)
               (set! x new-x)))
            (else (error "unknown request"))))
    dispatch))
