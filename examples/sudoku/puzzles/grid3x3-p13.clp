;;; The puzzle is: 
;;;
;;;    5 * *  * * *  * * 1
;;;    * * *  * * *  9 4 3
;;;    * * 4  * 1 *  * 7 *
;;;
;;;    * * *  4 6 *  1 * *
;;;    * 5 *  2 * 9  * 6 *
;;;    * * 8  * 3 5  * * *
;;;
;;;    * 9 *  * 2 *  3 * *
;;;    7 6 5  * * *  * * *
;;;    4 * *  * * *  * * 9
;;;
;;; The solution is: 
;;;
;;;    5 7 9  3 4 6  8 2 1
;;;    2 1 6  5 8 7  9 4 3
;;;    3 8 4  9 1 2  5 7 6
;;;
;;;    9 2 7  4 6 8  1 3 5
;;;    1 5 3  2 7 9  4 6 8
;;;    6 4 8  1 3 5  7 9 2
;;;
;;;    8 9 1  6 2 4  3 5 7
;;;    7 6 5  8 9 3  2 1 4
;;;    4 3 2  7 5 1  6 8 9
;;;
;;; Rules used:
;;;
;;;    Naked Single
;;;    Hidden Single
;;;    Locked Candidate Single Line
;;;    Hidden Pairs
;;;    Naked Triples
;;;    Swordfish
;;;    Color Conjugate Pairs

(defrule grid-values

   ?f <- (phase grid-values)

   =>
   
   (retract ?f)
   
   (assert (phase expand-any))

   (assert (size 3))

   (assert (possible (row 1) (column 1) (value 5) (group 1) (id 1)))
   (assert (possible (row 1) (column 2) (value any) (group 1) (id 2)))
   (assert (possible (row 1) (column 3) (value any) (group 1) (id 3)))
   (assert (possible (row 2) (column 1) (value any) (group 1) (id 4)))
   (assert (possible (row 2) (column 2) (value any) (group 1) (id 5)))
   (assert (possible (row 2) (column 3) (value any) (group 1) (id 6)))   
   (assert (possible (row 3) (column 1) (value any) (group 1) (id 7)))
   (assert (possible (row 3) (column 2) (value any) (group 1) (id 8)))
   (assert (possible (row 3) (column 3) (value 4) (group 1) (id 9)))   

   (assert (possible (row 1) (column 4) (value any) (group 2) (id 10)))
   (assert (possible (row 1) (column 5) (value any) (group 2) (id 11)))
   (assert (possible (row 1) (column 6) (value any) (group 2) (id 12)))
   (assert (possible (row 2) (column 4) (value any) (group 2) (id 13)))
   (assert (possible (row 2) (column 5) (value any) (group 2) (id 14)))
   (assert (possible (row 2) (column 6) (value any) (group 2) (id 15)))
   (assert (possible (row 3) (column 4) (value any) (group 2) (id 16)))
   (assert (possible (row 3) (column 5) (value 1) (group 2) (id 17)))
   (assert (possible (row 3) (column 6) (value any) (group 2) (id 18)))

   (assert (possible (row 1) (column 7) (value any) (group 3) (id 19)))
   (assert (possible (row 1) (column 8) (value any) (group 3) (id 20)))
   (assert (possible (row 1) (column 9) (value 1) (group 3) (id 21)))
   (assert (possible (row 2) (column 7) (value 9) (group 3) (id 22)))
   (assert (possible (row 2) (column 8) (value 4) (group 3) (id 23)))
   (assert (possible (row 2) (column 9) (value 3) (group 3) (id 24)))
   (assert (possible (row 3) (column 7) (value any) (group 3) (id 25)))
   (assert (possible (row 3) (column 8) (value 7) (group 3) (id 26)))
   (assert (possible (row 3) (column 9) (value any) (group 3) (id 27)))   

   (assert (possible (row 4) (column 1) (value any) (group 4) (id 28)))
   (assert (possible (row 4) (column 2) (value any) (group 4) (id 29)))
   (assert (possible (row 4) (column 3) (value any) (group 4) (id 30)))
   (assert (possible (row 5) (column 1) (value any) (group 4) (id 31)))
   (assert (possible (row 5) (column 2) (value 5) (group 4) (id 32)))
   (assert (possible (row 5) (column 3) (value any) (group 4) (id 33)))
   (assert (possible (row 6) (column 1) (value any) (group 4) (id 34)))
   (assert (possible (row 6) (column 2) (value any) (group 4) (id 35)))
   (assert (possible (row 6) (column 3) (value 8) (group 4) (id 36)))   
   
   (assert (possible (row 4) (column 4) (value 4) (group 5) (id 37)))
   (assert (possible (row 4) (column 5) (value 6) (group 5) (id 38)))
   (assert (possible (row 4) (column 6) (value any) (group 5) (id 39)))
   (assert (possible (row 5) (column 4) (value 2) (group 5) (id 40)))
   (assert (possible (row 5) (column 5) (value any) (group 5) (id 41)))
   (assert (possible (row 5) (column 6) (value 9) (group 5) (id 42)))
   (assert (possible (row 6) (column 4) (value any) (group 5) (id 43)))
   (assert (possible (row 6) (column 5) (value 3) (group 5) (id 44)))
   (assert (possible (row 6) (column 6) (value 5) (group 5) (id 45)))

   (assert (possible (row 4) (column 7) (value 1) (group 6) (id 46)))
   (assert (possible (row 4) (column 8) (value any) (group 6) (id 47)))
   (assert (possible (row 4) (column 9) (value any) (group 6) (id 48)))
   (assert (possible (row 5) (column 7) (value any) (group 6) (id 49)))
   (assert (possible (row 5) (column 8) (value 6) (group 6) (id 50)))
   (assert (possible (row 5) (column 9) (value any) (group 6) (id 51)))
   (assert (possible (row 6) (column 7) (value any) (group 6) (id 52)))
   (assert (possible (row 6) (column 8) (value any) (group 6) (id 53)))
   (assert (possible (row 6) (column 9) (value any) (group 6) (id 54)))   

   (assert (possible (row 7) (column 1) (value any) (group 7) (id 55)))
   (assert (possible (row 7) (column 2) (value 9) (group 7) (id 56)))
   (assert (possible (row 7) (column 3) (value any) (group 7) (id 57)))
   (assert (possible (row 8) (column 1) (value 7) (group 7) (id 58)))
   (assert (possible (row 8) (column 2) (value 6) (group 7) (id 59)))
   (assert (possible (row 8) (column 3) (value 5) (group 7) (id 60)))
   (assert (possible (row 9) (column 1) (value 4) (group 7) (id 61)))
   (assert (possible (row 9) (column 2) (value any) (group 7) (id 62)))
   (assert (possible (row 9) (column 3) (value any) (group 7) (id 63)))   
   
   (assert (possible (row 7) (column 4) (value any) (group 8) (id 64)))
   (assert (possible (row 7) (column 5) (value 2) (group 8) (id 65)))
   (assert (possible (row 7) (column 6) (value any) (group 8) (id 66)))
   (assert (possible (row 8) (column 4) (value any) (group 8) (id 67)))
   (assert (possible (row 8) (column 5) (value any) (group 8) (id 68)))
   (assert (possible (row 8) (column 6) (value any) (group 8) (id 69)))
   (assert (possible (row 9) (column 4) (value any) (group 8) (id 70)))
   (assert (possible (row 9) (column 5) (value any) (group 8) (id 71)))
   (assert (possible (row 9) (column 6) (value any) (group 8) (id 72)))

   (assert (possible (row 7) (column 7) (value 3) (group 9) (id 73)))
   (assert (possible (row 7) (column 8) (value any) (group 9) (id 74)))
   (assert (possible (row 7) (column 9) (value any) (group 9) (id 75)))
   (assert (possible (row 8) (column 7) (value any) (group 9) (id 76)))
   (assert (possible (row 8) (column 8) (value any) (group 9) (id 77)))
   (assert (possible (row 8) (column 9) (value any) (group 9) (id 78)))
   (assert (possible (row 9) (column 7) (value any) (group 9) (id 79)))
   (assert (possible (row 9) (column 8) (value any) (group 9) (id 80)))
   (assert (possible (row 9) (column 9) (value 9) (group 9) (id 81))))   
