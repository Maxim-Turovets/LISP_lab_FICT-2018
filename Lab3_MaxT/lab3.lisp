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

;Task 2
(defun deriv (e) 
    (cond ((null e) 0) 
          ((equal e 'x) 1)
          ((atom e) 0)
          ((null (cdr e)) (deriv (car e)))
          ((null (cddr e)); monadic operator +, -, or function id
          (cond ((equal (car e) '+ ) (deriv (cadr e))) ;+
          ((equal (car e) '- ) (list '- (deriv (cadr e))));-
          (t (derfun (car e) (cadr e))) ) ) ; function
          (t (derexpr (car e) (cadr e) (caddr e))) 
    )
)

(defun derexpr (arg1 op arg2 )
    (cond ((equal op '+ ) (deradd arg1 arg2 ))
          ((equal op '- ) (dersub arg1 arg2 ))
          ((equal op '* ) (dermult arg1 arg2))
          ((equal op '/ ) (derdiv arg1 arg2))
          ((equal op '^ ) (derpower arg1 arg2))
          (t (print 'error)) 
    )
)

(defun derfun (fun arg)
    (cond ((equal 'SIN fun) (list (list 'COS arg) '* (deriv arg) ))
          ((equal 'COS fun) (list (list '- (list 'SIN arg)) '*
          (deriv arg) ))
          ((equal 'EXP fun) (list (list 'EXP (list arg)) '*
          (deriv arg) ))
          ((equal 'LOG fun) (list (deriv arg) '/ arg ))
          (t (print 'illegal_function)) 
    )
)

(defun deradd (arg1 arg2)
    (list (deriv arg1) '+ (deriv arg2))
)

(defun dersub (arg1 arg2)
    (list (deriv arg1) '- (deriv arg2))
)

(defun derdiv (arg1 arg2)
    (list (list (list (deriv arg1) '* arg2)
     '- (list arg1 '* (deriv arg2) ))
     '/ (list arg2 '^ '2)
    )
)

(defun dermult (arg1 arg2)
    (list (list (deriv arg1) '* arg2)
     '+ (list arg1 '* (deriv arg2)) 
    )
)

(defun derpower (arg1 arg2)
    (list (list arg1 '^ arg2)
     '* (dermult arg2 (list 'LOG(list arg1)))
    )
)

(print(deriv '(SIN x)))

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
