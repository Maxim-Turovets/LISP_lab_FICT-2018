;MaxT 
(setq l1 '(Z X C S A D F))
(setq l2 '((R) (30) (3) 23))
(setq l3 '(U I 8 9 6 5 4 3 (1 2 3)))

; Task 1

(print(
(lambda (l1 l2 l3)
   (cons (car l1) (cons (car l2) (cons (car l3)nil)))) 
   l1 l2 l3))
   
; Task 2 
(DEFUN CONCAT_LISTS  (l1 l2 l3) 
       (list (NTH 5 l1) (NTH 4 l2) (NTH 7 l3))
)
(print (CONCAT_LISTS l1 l2 l3))  

; Task 3
(setq l4 '(9 2 3 4 5 12 34 65 34 2 39))

(setq cr (car l4))
(setq z (evenp(car l4)))
(setq s t)

(DEFUN FUN (j l4)
 (if (= 0 (mod cr 2))
   (Loop for i from 1 to (length l4)
         for a in l4
         do
         (if (> 4 i)
         (print (* a a))
         (print a))
         )
    
    ;else     
  (Loop for i from 1 to (length l4)
         for a in l4
         do
         (if (> 4 i)
         (print (* a a a))
         (print a))
         )         
 )
)

(FUN z l4)
 





    

