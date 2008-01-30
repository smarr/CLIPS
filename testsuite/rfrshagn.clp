;;;************************************************************
;;; REFRESH AGENDA ERROR CHECKING
;;;
;;; This file tests the refresh agenda function.
;;;
;;; To test perform a (clear), (load ...), and (testit)
;;; Note that a (clear) must be performed before rerunning
;;; this test because the initial when-defined salience values
;;; will have been lost.
;;;************************************************************

(defglobal ?*x* = 2)
(defglobal ?*y* = 3)
(defglobal ?*z* = 4)
(defglobal ?*which-test* = 1)

(deffunction testit ()
  (unwatch all)
  (set-strategy depth)
  (set-salience-evaluation when-defined)
  (printout t "Testing Standard Rule Execution..." crlf)
  (reset)
  (bind ?*which-test* 1)
  (run)
  (printout t "Testing Single External Refresh Agenda..." crlf)
  (reset)
  (bind ?*which-test* 2)
  (bind ?*x* 3)
  (bind ?*y* 4)
  (bind ?*z* 2)
  (refresh-agenda)
  (run)
  (printout t "Testing Refresh Agenda Within Rules..." crlf)
  (reset)
  (bind ?*which-test* 3)
  (bind ?*x* -1)
  (bind ?*y* -1)
  (bind ?*z* -1)
  (refresh-agenda)
  (run))

(deffunction print-order ($?x)
  (bind ?r (nth ?*which-test* ?x))
  (printout t "This rule should fire " ?r crlf))

;;;**************************
;;; RULE 1 - EVALUATION ORDER
;;; test 1   - 4th  
;;; test 2   - 3rd
;;; test 3   - 2nd
;;;**************************
(defrule rule-1 ""
   (declare (salience ?*x*))
   =>
   (bind ?*z* 3)
   (if (= ?*which-test* 3) then (refresh-agenda))
   (print-order 4th 3rd 2nd))

;;;**************************
;;; RULE 2 - EVALUATION ORDER
;;; test 1   - 3rd  
;;; test 2   - 2nd 
;;; test 3   - 4th
;;;**************************
(defrule rule-2 ""
   (declare (salience ?*y*))
   =>
   (if (= ?*which-test* 3) then (refresh-agenda))
   (print-order 3rd 2nd 4th))

;;;**************************
;;; RULE 3 - EVALUATION ORDER
;;; test 1   - 2nd  
;;; test 2   - 4th 
;;; test 3   - 3th
;;;**************************
(defrule rule-3 ""
   (declare (salience ?*z*))
   =>
   (bind ?*y* 2)
   (if (= ?*which-test* 3) then (refresh-agenda))
   (print-order 2nd 4th 3rd))

;;;**************************
;;; RULE 4 - EVALUATION ORDER
;;; test 1   - 1st  
;;; test 2   - 1st
;;; test 3   - 1st
;;;**************************
(defrule rule-4 ""
   (declare (salience 5))
   =>
   (bind ?*x* 4)
   (if (= ?*which-test* 3) then (refresh-agenda))
   (print-order 1st 1st 1st))

;;;**************************
;;; RULE 5 - EVALUATION ORDER
;;; test 1   - 5th  
;;; test 2   - 5th 
;;; test 3   - 5th
;;;**************************
(defrule rule-5 ""
   (declare (salience 1))
   =>
   (if (= ?*which-test* 3) then (refresh-agenda))
   (print-order 5th 5th 5th))

