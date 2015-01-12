(define (close-enough? v1 v2) 
  (define tolerance 1.e-6) 
  (< (/ (abs (- v1 v2)) v2)  tolerance)) 
  
(define (iterative-improve improve close-enough?) 
  (lambda (x) 
    (let ((xim (improve x))) 
      (if (close-enough? x xim) 
          xim 
          ((iterative-improve improve close-enough?) xim))))) 
  
 ; (a) rewrite sqrt using iterative-improve 
(define (sqrt x) 
  ((iterative-improve   
    (lambda (y) 
      (/ (+ (/ x y) y) 2)) 
    close-enough?) 1.0)) 
  
 ; (b) rewrite fixed-point using iterative-improve 
(define (fixed-point f first-guess) 
  ((iterative-improve 
     ; improve function is nothing but the 
     ; function f whose fixed point is to be found! 
    f 
    close-enough?) first-guess)) 
