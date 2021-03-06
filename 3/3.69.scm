(define (display-line str)
  (display str)
  (newline))

(define (display-stream str num)
  (define (internal index)
    (if (>= index num) 'printed
        (begin
          (display-line (stream-ref str index))
          (internal (+ 1 index)))))
  (internal 0))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (integers-from-n n)
  (cons-stream n (integers-from-n (+ 1 n))))

(define integers (integers-from-n 1))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s)  x))
                (pairs t (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define pythagorean-triples
  (stream-filter (lambda (x)
                   (= (square (caddr x))
                      (+ (square (car x)) (square (cadr x)))))
                 (triples integers integers integers)))
