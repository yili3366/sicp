(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))


(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (partial-sums s)
  (define result (cons-stream (stream-ref s 0)
                              (add-streams (stream-cdr s) result)))
  result)

(define (add-streams s1 s2)
  (stream-map + s1 s2))


(define (ln-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-summands (+ n 1)))))

(define ln-stream
  (partial-sums (ln-summands 1)))

;(stream-head ln-stream 10)
;(stream-head (euler-transform ln-stream) 10)
(stream-head (accelerated-sequence
              euler-transform
              ln-stream) 10)
