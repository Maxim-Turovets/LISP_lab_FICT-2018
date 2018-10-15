:Task 1
; lambda
(defun fact_lambda (n)
  ((lambda (n)
    (cond
      ((eq n 1) 1)
      (t (* n (fact_lambda (- n 1))))))
  n)
)

; let
(defun fact_let (n)
  (let ((n
      (cond
        ((eq n 1) 1)
        (t (* n (fact_let (- n 1))))
      )
    ))
    n)
)
(print(fact_lambda 3))
(print(fact_let 3))

;Task 3
    (defun ins (l1 l2 ct)
    ((lambda (list1 list2 count)
        (cond ((null list1) (append list2 nil))
        ((> count 0) (cons (car list1) (ins (cdr list1) list2 (- count 1))))
        ((append list2  list1))))
    l1 l2 ct )   
)
        
       
                     ;list1     list2   count
       (print( ins '(1 2 3 4) '(5 6 7 8) 4))


:Task 6
(defun sw (s)
  (map 'list #'string s))
 
(defun az (w a b)
  (cond ((null w) nil)
        ((and (string= (car w) a) (string= (caddr w) b))
         (cons (car w) (az (cddr w) a b)))
        ((cons (car w) (az (cdr w) a b)))))
 
(defun cn (w)
  (cond ((null w) nil)
        ((concatenate 'string (car w) (cn (cdr w))))))
 
(defun a-delete-b (s a b)
  (cn (az (sw s) a b)))
 
(print (a-delete-b "abcq2w abq9wc" "q" "w"))
