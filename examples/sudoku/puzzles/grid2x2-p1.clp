

(defrule grid-values

   ?f <- (phase grid-values)

   =>
   
   (retract ?f)
   
   (assert (phase expand-any))

   (assert (size 2))
   
   (assert (possible (row 1) (column 1) (value any) (group 1) (id 1)))
   (assert (possible (row 1) (column 2) (value any) (group 1) (id 2)))
   (assert (possible (row 2) (column 1) (value 2) (group 1) (id 3)))
   (assert (possible (row 2) (column 2) (value any) (group 1) (id 4)))

   (assert (possible (row 1) (column 3) (value 1)   (group 2) (id 10)))
   (assert (possible (row 1) (column 4) (value any) (group 2) (id 11)))
   (assert (possible (row 2) (column 3) (value any) (group 2) (id 12)))
   (assert (possible (row 2) (column 4) (value any) (group 2) (id 13)))

   (assert (possible (row 3) (column 1) (value any) (group 3) (id 19)))
   (assert (possible (row 3) (column 2) (value any) (group 3) (id 20)))
   (assert (possible (row 4) (column 1) (value any)   (group 3) (id 21)))
   (assert (possible (row 4) (column 2) (value 2)   (group 3) (id 22)))

   (assert (possible (row 3) (column 3) (value any)   (group 4) (id 28)))
   (assert (possible (row 3) (column 4) (value 4)   (group 4) (id 29)))
   (assert (possible (row 4) (column 3) (value any)   (group 4) (id 30)))
   (assert (possible (row 4) (column 4) (value any) (group 4) (id 31))))   
