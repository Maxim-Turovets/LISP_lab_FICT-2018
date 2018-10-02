

; Task 3
(setq l4 '(1 4 3))

(DEFUN FUN ( l4)
(setq one  (car l4))
(setq two (car(cdr l4)))
(setq three (car(cddr l4)))

       (if (= 1 (mod one 2) )
         (if (= 1 (mod two 2) )
           (if (= 1 (mod three 2) )
            (print (list  (* one one one) (* two two two) (* three three three)))
            (print (+ one two three))
           )
           (print (+ one two three))
         ) 
        (print (+ one two three))
        ) 
)
(FUN l4)


           
         