; Task 1
(defun ins (list1 list2 count)
  (cond ((null list1) (append list2 nil))
        ((> count 0) (cons (car list1) (ins (cdr list1) list2 (- count 1))))
        ((append list2  list1))))
        
                     ;list1     list2   count
       (print( ins '(1 2 3 4) '(5 6 7 8) 4))