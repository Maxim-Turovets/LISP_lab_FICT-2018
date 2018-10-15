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
