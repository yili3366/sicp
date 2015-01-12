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

(define zero-crossings
  (stream-map sign-change-detector sense-data 
              (cons-stream 0 sense-data)))

