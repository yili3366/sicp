#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
 
(define nil '())
 
(define one 0.99)
 
(define origin (make-vect 0 0))
 
(define lower-right (make-vect one 0))
 
(define upper-left (make-vect 0 one))
 
(define upper-right (make-vect one one))

;; (define (outline frame)
;;   ((segments->painter (list (make-segment origin lower-right)
;;                             (make-segment lower-right upper-right)
;;                             (make-segment upper-right upper-left)
;;                             (make-segment upper-left origin)))))

;; (paint outline)

(define (connect vect-list)
  (define (iter segment-list remaining)
    (if (null? (cdr remaining))
        (reverse segment-list)
        (iter (cons (make-segment (car remaining)
                                     (cadr remaining))
                       segment-list)
                 (cdr remaining))))
  (iter nil vect-list))

(define (outline frame)
  ((segments->painter (connect (list origin
                                     lower-right
                                     upper-right
                                     upper-left
                                     origin)))
   frame))

(paint outline)

(define (x-marks-the-spot frame)
  ((segments->painter (list (make-segment origin upper-right)
                            (make-segment lower-right upper-left)))
   frame))

(paint x-marks-the-spot)

(define (diamond frame)
  (let ((start (make-vect 0.5 0)))
    ((segments->painter (connect (list start
                                       (make-vect one 0.5)
                                       (make-vect 0.5 one)
                                       (make-vect 0 0.5)
                                       start)))
     frame)))

(paint diamond)

(define (wave frame)
  ((segments->painter (append (connect (list (make-vect 0.4  0.0)
                                             (make-vect 0.5  0.33)
                                             (make-vect 0.6  0.0))) ;inside legs
                              (connect (list (make-vect 0.25 0.0)
                                             (make-vect 0.33 0.5)
                                             (make-vect 0.3  0.6)
                                             (make-vect 0.1  0.4)
                                             (make-vect 0.0  0.6))) ;lower left
                              (connect (list (make-vect 0.0  0.8)
                                             (make-vect 0.1  0.6)
                                             (make-vect 0.33 0.65)
                                             (make-vect 0.4  0.65)
                                             (make-vect 0.35 0.8)
                                             (make-vect 0.4  1.0))) ;upper left
                              (connect (list (make-vect 0.75 0.0)
                                             (make-vect 0.6  0.45)
                                             (make-vect 1.0  0.15)));lower right
                              (connect (list (make-vect 1.0  0.35)
                                             (make-vect 0.8  0.65)
                                             (make-vect 0.6  0.65)
                                             (make-vect 0.65 0.8)
                                             (make-vect 0.6  1.0)))));upper right
   frame))

(paint wave)
