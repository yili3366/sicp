(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(count-pairs (list 'a 'b 'c))  ;;=> 3
(define second (cons 'a 'b))
(define third (cons 'a 'b))
(define first (cons second third))
(set-car! third second)
(count-pairs first)  ;;=> 4

(define third (cons 'a 'b))
(define second (cons third third))
(define first (cons second second))
(count-pairs first)  ;;=> 7

(define lst (list 'a 'b 'c))
(set-cdr! (cddr lst) lst)
(count-pairs lst)  ;;never returns
