
    
    
;http://lisper.ru/pcl/practical-a-simple-database

;Выставки и конференции

(defun make-ts (title type style)
(list :title title :type type :style style))

;глобальна змінна
(defvar *db* nil)

;додавання запису в базу даних
(defun add-record (ts) (push ts *db*))

;виводить зміст бази данних в більш читабельній формі
(defun dump-db ()
(dolist (ts *db*)
(format t "~{~a:~10t~a~%~}~%" ts)))

; Выбирает некоторое значение из базы данных
(defun select (selector-fn)
(remove-if-not selector-fn *db*))

; Выбирает тип выставки
(defun type-selector (type)
(lambda (ts) (equal (getf ts :type) type)))

; Генерирует выражение выбора, которое возвращает все записи о выставках, которые совпадают со значениями, заданными в where
(defun where (&key title type style)
(lambda (ts)
(and
(if title (equal (getf ts :title) title) t)
(if type (equal (getf ts :type) type) t)
(if style (equal (getf ts :style) style) t))))

; Обновления и использования аргументов-ключей для предоставления нового значения
(defun update (selector-fn &key title type style (ripped nil ripped-p))
(setf *db*
(mapcar
(lambda (row)
(when (funcall selector-fn row)
(if title (setf (getf row :title) title))
(if type (setf (getf row :type) type))
(if style (setf (getf row :style) style)))
row) *db*)))

;Удаление строк из базы данных
(defun delete-rows (selector-fn)
(setf *db* (remove-if selector-fn *db*)))

; Поиск по заданным значениям
(defun make-comparison-expr (field value)
(list 'equal (list 'getf 'ts field) value))

; добавление значений в базу
 (print "Add info")
 (add-record (make-ts "Flowers" "Сreative" "Modern"))
 (add-record (make-ts "Pictures" "Сreative" "PostModern"))
 (add-record (make-ts "Electronics" "Scientific" "Nowadays"))
 (dump-db)
 
 ;  поиск
 (print "Search 1")
 (print(select(type-selector "Scientific")))
 (print "Search 2")
 (print(select(where :title "Flowers")))
 
 ;обновление данных
 (print "Update")
 (print(update(where :style "PostModern") :style "Avangard"))
 
  ; удаление 
 (print "Delete")
 (print (delete-rows(where :title "Pictures" )))
