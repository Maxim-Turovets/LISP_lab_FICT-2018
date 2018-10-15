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
