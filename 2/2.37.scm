(define nil (quote ()))
 
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (vector-*-matrix v m)
  (map (lambda (row) (dot-product v row)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (transpose (map (lambda (v) (matrix-*-vector m v))
                    cols))))

(define (matrix-*-matrix m n)
  (map (lambda (v) (vector-*-matrix v (transpose n))) m))
