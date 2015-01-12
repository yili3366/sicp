(load "3.60.scm")
(load "3.61.scm")

(define (div-series num denom)
  (let ((denom-const (stream-car denom)))
    (if (zero? denom-const)
        (error ("DIV-SERIES -- denominator constant term must be non-zero"))
        (mul-series num
                    (scale-stream         ; restore the scaling factor
                     (invert-unit-series  ; requires a stream that has a unit
                                          ; constant term
                      (scale-stream denom (/ 1 denom-const)))
                     denom-const)))))

(define tane-series (div-series sine-series cosine-series))
