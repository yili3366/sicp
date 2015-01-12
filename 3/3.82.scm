(load "display-stream.scm")

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

(define (random-in-range-stream low high)
  (let ((range (- high low)))
    (define (random-in-range low high)
      ;; with a bit improved resolution
      (+ low (/ (random (inexact->exact (* range 100))) 100)))
    (cons-stream
     (random-in-range low high)
     (random-in-range-stream low high))))

;; some predicates for the circles
(define (central-unit-circle x y)
  (<= (+ (* x x) (* y y)) 1))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream (/ passed (+ passed failed))
                 (monte-carlo
                  (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (estimate-integral predicate x1 x2 y1 y2)
  (define rectangle-area
    (* (- x2 x1) (- y2 y1)))
  (define experiment-stream
    (stream-map predicate
                (random-in-range-stream x1 x2)
                (random-in-range-stream y1 y2)))
  (stream-map (lambda (success-percentage)
                (* success-percentage rectangle-area))
              (monte-carlo experiment-stream 0 0)))

;;test
(stream-ref (estimate-integral central-unit-circle -1.0 1.0 -1.0 1.0) 50000)
