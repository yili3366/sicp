(load "display-stream.scm")

(define sense-data
  (list->stream
   (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4 0 0 0 0 0 -1 0 0 0 0 1 0 0)))

(define (sign-change-detector current previous)
  (cond ((and (>= current 0)
              (< previous 0)) 1)
        ((and (< current 0)
              (>= previous 0)) -1)
        (else 0)))

(define (smooth stream)
  (stream-map (lambda (x y)
                (/ (+ x y) 2))
              stream
              (cons-stream 0 stream)))

(define (make-zero-crossings input-stream)
  (let ((smoothed-values (smooth input-stream)))
    (stream-map sign-change-detector
                smoothed-values
                (cons-stream 0 smoothed-values))))

(define zero-crossings
  (make-zero-crossings (smooth sense-data)))
