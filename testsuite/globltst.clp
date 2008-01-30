;;;************************************************************
;;; DEFGLOBAL EXECUTION CHECKING
;;;
;;; This file tests the usage of global variables. It checks
;;; their usage in the following constructs
;;;   1)  defrule (LHS, RHS, and test CE)
;;;   2)  deffunction
;;;   3)  deffacts
;;;
;;; To test perform a (clear), (load ...), (reset), and (run)
;;;************************************************************

(defglobal ?*x* = 3
           ?*y* = (create$ a b c)
           ?*z* = 5
           ?*w* = (+ ?*x* ?*z*))

(deffacts info
   (fact-1 ?*x*)
   (fact-2 ?*y*)
   (fact-3 ?*z*)
   (fact-4 ?*w*))

(deffunction printem (?x ?y ?z ?w)
   (printout t "Passed values: |" 
               ?x "|" ?y "|" ?z "|" ?w "|" crlf)
   (printout t "Global values: |" 
               ?*x* "|" ?*y* "|" ?*z* "|" ?*w* "|" crlf))

(defrule rule-1
   (fact-1 ?x)
   (fact-2 $?y)
   (fact-3 ?z)
   (fact-4 ?w)
   =>
   (printout t "*********************************" crlf)
   (printout t "This should be the first test" crlf)
   (printout t "All values should appear the same" crlf)
   (printout t "               |3|(a b c)|5|8|" crlf)
   (printout t "*********************************" crlf)
   (printout t "rule-1 values: |" 
               ?x "|" ?y "|" ?z "|" ?w "|" crlf)
   (printout t "rule-1 values: |" 
               ?*x* "|" ?*y* "|" ?*z* "|" ?*w* "|" crlf)
   (printem ?x ?y ?z ?w)
   (printem ?*x* ?*y* ?*z* ?*w*)
   (assert (fired rule-1)))

(defrule rule-2
   (fired rule-1)
   ?f1 <- (fact-1 3)
   ?f2 <- (fact-2 a b c)
   ?f3 <- (fact-3 5)
   ?f4 <- (fact-4 8)
   =>
   (retract ?f1 ?f2 ?f3 ?f4)
   (printout t "*********************************" crlf)
   (printout t "This should be the second test" crlf)
   (printout t "No printout is associated" crlf)
   (printout t "*********************************" crlf)
   (bind ?*x* (create$ d e f g))
   (bind ?*y* 9)
   (bind ?*z* (create$ h i j))
   (bind ?*w* (create$ ?*x* ?*z*))
   (assert (fact-1 ?*x*))
   (assert (fact-2 ?*y*))
   (assert (fact-3 ?*z*))
   (assert (fact-4 ?*w*))
   (assert (fired rule-2)))

(defrule rule-3
   (fired rule-2)
   (fact-1 $?x)
   (fact-2 ?y)
   (fact-3 $?z)
   (fact-4 $?w)
   =>
   (printout t "*********************************" crlf)
   (printout t "This should be the third test" crlf)
   (printout t "All values should appear the same" crlf)
   (printout t "               |(d e f g)|9|(h i j)|(d e f g h i j)|" crlf)
   (printout t "*********************************" crlf)
   (printout t "rule-3 values: |" 
               ?x "|" ?y "|" ?z "|" ?w "|" crlf)
   (printout t "rule-3 values: |" 
               ?*x* "|" ?*y* "|" ?*z* "|" ?*w* "|" crlf)
   (printem ?x ?y ?z ?w)
   (printem ?*x* ?*y* ?*z* ?*w*)
   (assert (fired rule-3)))

(defrule rule-4
   (fired rule-3)
   ?f1 <- (fact-1 $?x)
   (test (and (eq ?x (create$ d e f g))
              (eq ?x ?*x*)))
   ?f2 <- (fact-2 ?y)
   (test (and (= ?*y* 9)
              (eq ?y 9)
              (eq ?y ?*y*)))
   ?f3 <- (fact-3 $?z)
   (test (and (eq ?z (create$ h i j))
              (eq ?z ?*z*)))
   ?f4 <- (fact-4 $?w)
   (test (and (eq ?w (create$ ?x ?z))
              (eq ?*w* ?w)))
   =>
   (retract ?f1 ?f2 ?f3 ?f4)
   (printout t "*********************************" crlf)
   (printout t "This should be the fourth test" crlf)
   (printout t "No printout is associated" crlf)
   (printout t "*********************************" crlf)
   (assert (fired rule-4)))

(defrule test-completion
   (fired rule-1)
   (fired rule-2)
   (fired rule-3)
   (fired rule-4)
   =>
   (printout t "*********************************" crlf)
   (printout t "Test Completed - No Errors" crlf)
   (printout t "*********************************" crlf)
   (halt))

(defrule test-errors
   (declare (salience -10))
   =>
   (printout t "*********************************" crlf)
   (printout t "Test Completed - Errors detected" crlf)
   (printout t "*********************************" crlf))