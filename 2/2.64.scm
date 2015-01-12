(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements) 
  (car (partial-tree elements (length elements)))) 

(define (partial-tree elts n) 
  (if (= n 0) 
      (cons '() elts) 
      (let ((left-size (quotient (- n 1) 2))) 
        (let ((left-result (partial-tree elts left-size))) 
          (let ((left-tree (car left-result)) 
                (non-left-elts (cdr left-result)) 
                (right-size (- n (+ left-size 1)))) 
            (let ((this-entry (car non-left-elts)) 
                  (right-result (partial-tree (cdr non-left-elts) 
                                              right-size))) 
              (let ((right-tree (car right-result)) 
                    (remaining-elts (cdr right-result))) 
                (cons (make-tree this-entry left-tree right-tree) 
                      remaining-elts))))))))

;; a. PARTIAL-TREE splits the list ELTS into three parts: the median item THIS-ENTRY, the list of items less than the median, and the list of items greater than the median. It creates a binary tree whose root node is THIS-ENTRY, whose left subtree is the PARTIAL-TREE of the smaller elements, and whose right subtree is the PARTIAL-TREE of the larger elements.

;; b. At each step, PARTIAL-TREE splits a list of length n into two lists of approximate length n ÷ 2. The work done to split the list is (QUOTIENT (- N 1) 2) and (- N (+ LEFT-SIZE 1)), both of which take constant time. The work to combine the results is (MAKE-TREE THIS-ENTRY LEFT-TREE RIGHT-TREE), and is also constant. Therefore, the time to make the partial tree of a list of n elements is:

;; T (n) = 2T (n ÷ 2) + Θ (1)

;; By the Master Theorem, we have a = 2, b = 2, and f (n) = Θ (1). Therefore, T (n) = Θ (n).

;; The time taken by LIST->TREE for a list of length n will be the time taken by PARTIAL-TREE plus the time taken by LENGTH for that list. Both procedures have order of growth Θ (n), so the order of growth of LIST->TREE is Θ (n).
