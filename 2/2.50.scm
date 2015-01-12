#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
 
(define (my-flip-horiz painter)
  ((transform-painter (make-vect 1.0 0.0)
                      (make-vect 0.0 0.0)
                      (make-vect 1.0 1.0))
   painter))

(paint einstein)
(paint (my-flip-horiz einstein))

(define (my-rotate180 painter)
  ((transform-painter (make-vect 1.0 1.0)
                      (make-vect 0.0 1.0)
                      (make-vect 1.0 0.0))
   painter))

(paint (my-rotate180 einstein))

(define (my-rotate270 painter)
  ((transform-painter (make-vect 0.0 1.0)
                      (make-vect 0.0 0.0)
                      (make-vect 1.0 1.0))
   painter))

(paint (my-rotate270 einstein))
