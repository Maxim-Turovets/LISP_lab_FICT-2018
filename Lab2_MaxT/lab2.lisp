; Task 1
(defun ins (list1 list2 count)
  (cond ((null list1) (append list2 nil))
        ((> count 0) (cons (car list1) (ins (cdr list1) list2 (- count 1))))
        ((append list2  list1))))
        
                     ;list1     list2   count
       (print( ins '(1 2 3 4) '(5 6 7 8) 4))
	   
; Task 3
  (setq l4 '(1 2 3 8 5 4))
  
       
(defun is-sortedp (lst)
  (cond ((or (null lst) (null (cdr lst))) t)
        ((< (car lst) (car (cdr lst))) (is-sortedp (cdr lst)))
        (t nil)))


(defun bubble (lst)
  (cond ((or (null lst) (null (cdr lst))) lst)
        (( < (car lst) (car (cdr lst))) (cons (car lst) (bubble (cdr lst))))
        ( t (cons (car (cdr lst)) (bubble (cons (car lst) (cdr (cdr lst))))))))


(defun bubble-sort (lst)
  (cond ((or (null lst) (null (cdr lst))) lst)
        ((is-sortedp  lst) lst)
        (t (bubble-sort (bubble lst)))))
        
(print (bubble-sort l4) )	 

; Task 6
 (defun chars (s)
  (char-code s)
 )

(defun codes (w)
  (cond             
  ((null  w) t)
  ((listp (car w) )  (codes (car w))) ; является ли голова списком иначе рекурсия для головы
  ((print(chars (car w ))) (codes (cdr w ))) ; иначе рекурсивно для хвоста
  )
)
(defun chars (s)(loop for a across (string  s) collect (char-code a)))
(codes '(A d r h j u (a d c)))  
