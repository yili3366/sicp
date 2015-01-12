(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
;; (define (stream-map proc s)
;;   (if (stream-null? s)
;;       the-empty-stream
;;       (cons-stream (proc (stream-car s))
;;                    (stream-map proc (stream-cdr s)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

;; (define (display-stream s)
;;   (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (display-stream str num)
  (define (internal index)
    (if (>= index num) 'printed
        (begin
          (display-line (stream-ref str index))
          (internal (+ 1 index)))))
  (internal 0))

(define (display-stream-pair p num)
  (define (internal index)
    (if (> index num) 'printed
        (begin
          (display-line (list (stream-ref (car p) index) (stream-ref (cdr p) index)))
          (internal (+ 1 index)))))
  (newline)
  (internal 0))
