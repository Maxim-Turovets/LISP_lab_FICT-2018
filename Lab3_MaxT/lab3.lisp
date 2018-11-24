;Task 1
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

(defun defun (fun arg)
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

;Task 4

(defun intr (expr)
  (cond
    ((null expr) nil)
     ((atom expr) expr)
    ((eq (car expr) 'car)
      (car (intr (second expr))))
    ((eq (car expr) 'cdr)  
      (cdr (intr (second expr))))
    ((eq (car expr) '*)
      ( (intr (second expr)) (intr (third expr))))
    ((eq (car expr) '/) 
      (/ (intr (second expr)) (intr (third expr))))
    ((eq (car expr) '+)
      (+ (intr (second expr)) (intr (third expr))))
    ((eq (car expr) '-)
      (- (intr (second expr)) (intr (third expr))))
    ((eq (car expr) 'cons) (cons (intr (second expr)) (intr (third expr))))
      (t ())))


 (print(intr '(cons '(d g) '(l i s p))))
 (print(intr '(car (cdr '(r e s z c)))))
 (print(intr '(* (- 3 2) 8)))

;Task 5
(defun logarifm (num value)
 (cond
    ((null num) value)
    ((null value) num)
    (t (/(log num)(log value)) ))
    )

(print (logarifm 4 6))

;Task 6
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
  (cn (z (sw s) a b)))
 
(print (a-delete-b "abcq2w abq9wc" "q" "w"))

;Task 7
(let* (
		(phrase "Лисп самый легкий язык")
		(splitLetters "ауоэияюёеы")
		(splitL (lambda (fn chars strings)
		 (cond 
			((eq (length strings) 0)nil)
			((eq chars (char strings 0))t)
			(t(funcall fn fn chars (subseq strings 1)))
			)
		)
		)
		( getInd (lambda (fn word index)
							(cond 
								((eq index (length word))index)
								((funcall splitL splitL (char word index) splitLetters) 	(+ index 1))
								(t (funcall fn fn word (+ index 1)))
							)
						)
		)
		(splitWord (lambda (fn word)
						(cond 
							((eq (length word) 0) 	nil)
							(t (cons 
							   (subseq word 0 (funcall  getInd  getInd word 0))
							(funcall fn fn (subseq word (funcall  getInd  getInd word 0)))
							)
							)
						)
					)
		)
		(norm (lambda (slogs)
						(cond 
							((funcall splitL splitL 
							(char 
							(car (last slogs))
							(- (length (car (last slogs))) 1)
							) splitLetters ; Last symbol
								)	slogs
							)
							(t 
							(reverse (cons 
							(concatenate 'string 
							(second (reverse slogs)) 
							(car (reverse slogs))
							)
							(cddr (reverse slogs))
							)
							)
							)
						)
					)
		)
		(getormSlogs (lambda (word)
						(funcall norm (funcall splitWord splitWord word))
					)
		)
		(getIndexOfSpace (lambda (fn word index)
							(cond 
								((eq index (length word))index)
								((eq (char word index) #\Space)index)
								(t (funcall fn fn word (+ index 1)))
							)
						)
		)
		(splits (lambda (fn phrase)
						(cond 
							(	(eq 
									(funcall getIndexOfSpace getIndexOfSpace phrase 0) 
									(length phrase)
								)	(list phrase)
							)
							(t 
								(cons 
									(subseq phrase 0 (funcall getIndexOfSpace getIndexOfSpace phrase 0))
									(funcall fn fn (subseq phrase (+ 1 (funcall getIndexOfSpace getIndexOfSpace phrase 0))))
								)
							)
						)
					)
		)
	)

	(print (mapcar getNormSlogs (funcall splits splits phrase)))
	(terpri)
)

; Task8
(let* (
		(text "как-то летом на рассвете")	  
		(keyword "лень")
		(splitLetters "ауоэияюёеы")
		(splitL (lambda (fn chars strings)
						(cond 
							((eq (length strings) 0) nil)
							((eq chars (char strings 0))t)
							(t (funcall fn fn chars (subseq strings 1)))
						)
					)
		)
		( getnd (lambda (fn word index)
								(cond 
									((eq index (length word)) index)
									((funcall splitL splitL (char word index) splitLetters) 	(+ index 1))
									(t (funcall fn fn word (+ index 1)))
								)
							)
		)
		(getIndexOfSpace (lambda (fn word index)
							(cond 
								((eq index (length word))index)
								((eq (char word index) #\Space)index)
								(t (funcall fn fn word (+ index 1)))
							)
						)
		)
		(splits (lambda (fn phrase)
							(cond 
								(	(eq 
										(funcall getIndexOfSpace getIndexOfSpace phrase 0) 
										(length phrase)
									)	(list phrase)
								)
								(t 
									(cons 
										(subseq phrase 0 (funcall getIndexOfSpace getIndexOfSpace phrase 0))
										(funcall fn fn (subseq phrase (+ 1 (funcall getIndexOfSpace getIndexOfSpace phrase 0))))
									)
								)
							)
						)
		)
	)

	(print (mapcar
				(lambda (word)
					(concatenate 'string
						(subseq keyword 0	(funcall  getInd  getInd keyword 0))
						(subseq word (funcall  getInd  getInd word 0))
					)
				)
				(funcall splits splits text)
			)
	)
	(terpri)
)

;; Task 9
(let* (
		(getIndexOfSpace (lambda (fn word index)
								(cond 
									((eq index (length word)) index)
									((eq (char word index) #\Space)index)
									(t (funcall fn fn word (+ index 1)))
								)
						)
		)
		(splits (lambda (fn phrase)
							(cond 
								((eq (funcall getIndexOfSpace getIndexOfSpace phrase 0) (length phrase))
									(list phrase)
								)
								(t (cons (subseq phrase 0 (funcall getIndexOfSpace getIndexOfSpace phrase 0))
										 (funcall fn fn (subseq phrase
										 					(+ 1 (funcall getIndexOfSpace getIndexOfSpace phrase 0))
										 				)
										 )
									)
								)
							)
						)
		)
		(removEl (lambda (fn mylist el)
						(cond
							((null mylist)nil)
							((and (atom mylist) (eq mylist el))nil)
							((string= (car mylist) el) (funcall fn fn (cdr mylist) el))
							(t
								(cons (car mylist) (funcall fn fn (cdr mylist) el))
							)
						)
					) 
		)
		(countOfRepeting (lambda (fn lst word count)
							(cond
								((null lst)	(cons word count))
								((and (atom lst) (string= lst word))(cons word (+ count 1)))
								((string= (car lst) word)(funcall fn fn (cdr lst) word (+ count 1)))
								(t(funcall fn fn (cdr lst) word count))
							)
						)
		)
		(buildListOfRepeatingWords 
		(lambda (fn lst)
		 (cond
		 ((null lst)nil)
		  ((atom lst)		(list lst 1))
		(t(cons 
		(funcall countOfRepeting countOfRepeting (cdr lst) (car lst) 1) 
		(funcall fn fn (funcall removeEl removeEl lst (car lst)))
				)
			)		
		)
		)
		)
	)


	(print (funcall splits splits 
		"Приходит день, приходит час, и понимаешь: все не вечно.. Жизнь бессердечно учит нас о том, что время быстротечно..О том, что нужно все ценить,беречь все то, что нам дается.. Ведь жизнь - как тоненькая нить, она порой внезапно рвется.."))
	(print (funcall buildListOfRepeatingWords buildListOfRepeatingWords (funcall splits splits 
		"Приходит день, приходит час, и понимаешь: все не вечно.. Жизнь бессердечно учит нас о том, что время быстротечно..О том, что нужно все ценить,беречь все то, что нам дается.. Ведь жизнь - как тоненькая нить, она порой внезапно рвется.."))
	)
	(terpri)
)
