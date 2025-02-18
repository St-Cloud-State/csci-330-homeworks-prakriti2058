(defun insert-into-sorted (item sorted-list)
  
  (if (or (null sorted-list) (< item (first sorted-list)))
      (cons item sorted-list)  ;; if the item is smaller, place it at the beginning 
      (cons (first sorted-list) 
            (insert-into-sorted item (rest sorted-list)))))  ;; Recursively insert into the rest

(defun insertion-sort-helper (sorted unsorted)
  
  (if (null unsorted)  ;; Termination condition for unsorted list when it is empty
      sorted
      (insertion-sort-helper 
        (insert-into-sorted (first unsorted) sorted)  ;; Insert first element of unsorted list into sorted list
        (rest unsorted))))  ;; Move to the next element

(defun insertion-sort (lst)
  (insertion-sort-helper '() lst))  ;; Start with an empty sorted list
