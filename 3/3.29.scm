(load "3.21.scm")
(load "simulator.scm")

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and x y)
  (if (and (= x 1) (= y 1))
      1
      0))

(define (or-gate input-1 input-2 output)
  (let ((invert-1 (make-wire))
        (invert-2 (make-wire))
        (and-invert-1-invert-2 (make-wire)))
    (inverter input-1 invert-1)
    (inverter input-2 invert-2)
    (and-gate invert-1 invert-2 and-invert-1-invert-2)
    (inverter and-invert-1-invert-2 output))
  'ok)
