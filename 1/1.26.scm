;; Instead of a linear recursion, the rewritten expmod generates a tree 
;; recursion, whose execution time grows exponentially with the depth of the 
;; tree, which is the logarithm of N. Therefore, the execution time is linear 
;; with N. 
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base
                                    (- exp 1)
                                    m))
                    m))))
