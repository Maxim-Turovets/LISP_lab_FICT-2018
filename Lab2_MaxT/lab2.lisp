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
  (cond ((or (null lst) (null (cdr lst))) t) ; проверка не является ли список уже отсортированым
        ((< (car lst) (car (cdr lst))) (is-sortedp (cdr lst))) ; проверять рекурсивно
        (t nil)))


(defun bubble (lst)
  (cond ((or (null lst) (null (cdr lst))) lst) ; если один елемент или список пуст то закончить
        (( < (car lst) (car (cdr lst))) (cons (car lst) (bubble (cdr lst)))) ;если первый елемент больлше второго то поменять их местами (пузырек)
        ( t (cons (car (cdr lst)) (bubble (cons (car lst) (cdr (cdr lst)))))))) ; иначе работать с вторым и третьим рекурсивно и тд


(defun bubble-sort (lst)
  (cond ((or (null lst) (null (cdr lst))) lst) ; если список пустой или в нем только один елемент то вернуть список
        ((is-sortedp  lst) lst) ; если список уже отсортирован то вернуть его
        (t (bubble-sort (bubble lst))))) ;  иначе сортировать список
        
(print (bubble-sort l4) )	 

; Task 5
 (defun chars (s)  ; вытаскиваем  код символа
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
(codes '(#\d #\e (#\q #\s #\m)))  

; Task 2

(defun insertion (lst x)
  (cond ((null lst) (list x))
        ((> (car lst) x) (cons x lst))
        (T (cons (car lst) (insertion (cdr lst) x)))))

(defun isort (x &optional (s nil))
  (cond ((null x) s)
        (T (isort (cdr x) (insertion s (car x))))))

(defun shello (lst gap finalst)
  (cond ((null lst) finalst)
        (T (cond ((> gap (length lst)) (append finalst (isort lst)))
  (T (append finalst (isort (subseq lst 0 gap)) (shello (subseq lst gap (length lst)) gap finalst)))))))

(defun shellSort (lst gaps)
  (cond ((null (cdr gaps)) (shello lst (car gaps) '()))
        (T (shellSort (shello lst (car gaps) '()) (cdr gaps)))))

(defun gaps (len finalLIST)
  '(1 4 13 40 121 364))

(defun shellexecute (lst)
  (shellSort lst (gaps (length lst) '(1))))

(print (shellexecute '(4 2 8 3 1 7 5 6)))
