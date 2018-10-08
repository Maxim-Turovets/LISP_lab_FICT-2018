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

https://rosettacode.org/wiki/Sorting_algorithms/Shell_sort#Common_Lisp
; Task 2
(defun insertion (lst x) ; вставка елемента
  (cond ((null lst) (list x))  ; если список пуст то вернуть его
        ((> (car lst) x) (cons x lst)) ; если голова списка больше за вставляемый елемент то соеденить елемент и список
        (T (cons (car lst) (insertion (cdr lst) x))))) ; иначе вставить в хвост рекурсивно и соеденить с головой списка

(defun isort (x &optional (s nil)) ; список , необязательный параметр
  (cond ((null x) s) ;если список пуст , то оставить исходный список 
        (T (isort (cdr x) (insertion s (car x)))))) ; иначе поочередно c хвоста вставляем в список <s>  елементы списка <x>

(defun shello (lst gap finalst) ; сортировка с включением шага методом Кнута  (список шаг окончательный список)
  (cond ((null lst) finalst)  ; если список пуст то и окончательный будет таким же 
        (T (cond ((> gap (length lst)) (append finalst (isort lst))) ; если размер списка меньше очередного числа-шага 
         ;то соеденить окончательный список с отсортированым списком
  (T (append finalst (isort (subseq lst 0 gap)) (shello (subseq lst gap (length lst)) gap finalst)))))))
; subseq - (обрезать список от начала до шага сортировки)
;иначе соеденить окончательный список с отсортированым списком по заданому шагу рекурсивно

(defun shellSort (lst gaps); сортировка (список , список шагов сортировки)
  (cond ((null (cdr gaps)) (shello lst (car gaps) '())) ; если в списке больше одного елемента то выполнять рекурсивно сортировку
	  ; в качестве окончательного списка использовать пустой список
        (T (shellSort (shello lst (car gaps) '()) (cdr gaps)))))

(defun gaps (len finalLIST)   ; список (шаги сортировки Кнута)
  '(1 4 13 40 121 364))

(defun shellexecute (lst)
  (shellSort lst (gaps (length lst) '(1))))

(print (shellexecute '(4 2 8 3 1 7 5 6)))
