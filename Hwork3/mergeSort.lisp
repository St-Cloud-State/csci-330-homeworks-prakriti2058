(defun my-merge (list1 list2)
  (cond
    ((null list1) list2)
    ((null list2) list1)

    ;;compare the first elements of both lists
    (t (if (<= (car list1) (car list2))

        ;; If the first element of list1 is smaller or equal, 
        ;; add it to the result and merge the rest of list1 with list2
           (cons (car list1) (my-merge (cdr list1) list2))

           ;; If the first element of list2 is smaller, add it to the result and merge the rest of list2 with list1
           (cons (car list2) (my-merge list1 (cdr list2)))))))


;;split a list into two parts
(defun partition (lst)
  (if (or (null lst) (null (cdr lst)))

  ;; If list has 0 or 1 elements, return the list and an empty list
      (values lst '())

      ;; or split the list into two parts
      (let ((first (list (car lst)))
            (second (list (cadr lst))))

        ;;partition the rest of the list recursively
        (multiple-value-bind (l1 l2) (partition (cddr lst))

        ;; Return the merged result
          (values (append first l1) (append second l2))))))

(defun mergesort (lst)
  (if (or (null lst) (null (cdr lst)))

  ;;the list is sorted if it has 0 or 1 element
      lst
      
      ;;split the list into two halves and sort each half recursively
      (multiple-value-bind (first-half second-half) (partition lst)
        (my-merge (mergesort first-half) (mergesort second-half)))))

;; Testing mergesort
(print (mergesort '(5 2 9 1 5 6)))
(print (mergesort '(3 1 4 1 5 9 2 6 5 3 5)))
