;; Function for creating sorted pairs from the list
(defun make-sorted-pairs (lst)
  (if (null lst)
      nil ;; Base case,return nil if the list is empty
      (let ((first (car lst))  
            (rest (cdr lst)))
        (if (null rest) 
            (list (list first)) ;; Single-element list remains alone
            (cons (sort (list first (car rest)) #'<) 
                  (make-sorted-pairs (cdr rest))))))) ;; Recursively process the rest of the list


(defun merge-lists (l1 l2)  ;; Function that merges two sorted lists into one
  (cond
    ((null l1) l2)  ;; If the first list is empty, return the second
    ((null l2) l1)  ;; If the secondlist is empty, return the first
    ((< (car l1) (car l2)) ;; Compare first elements of both lists
    (cons (car l1) (merge-lists (cdr l1) l2)))  ;; Put smaller element first
    (t (cons (car l2) (merge-lists l1 (cdr l2)))))) 

(defun merge-pass (lst) ;; Function to merge lists iteratively
  (if (null lst)
      nil
      (if (null (cdr lst))
          lst    ;; If there's only one list left, return it as is
          (cons (merge-lists (car lst) (cadr lst))  ;; Merge the first two lists
                (merge-pass (cddr lst))))))  ;; Keep merging the remaining lists

(defun bottom-up-mergesort (lst)   ;; Function to implement bottom-up merge sort
  (let ((sublists (make-sorted-pairs lst))) ;; Create sorted pairs
    (loop while (> (length sublists) 1) do    ;; while loop until one sorted list remains
          (setf sublists (merge-pass sublists)))
    (car sublists)))   ;; Return the fully sorted list

;; Testing
(print (bottom-up-mergesort '(1 7 2 1 8 6 5 3 7 9 4)))
