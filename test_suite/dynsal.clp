;;;************************************************************
;;; DYNAMIC SALIENCE ERROR CHECKING
;;;
;;; This file tests the dynamic salience capability for
;;;   1) when-defined
;;;   2) when-activated
;;;   3) every-cycle
;;;
;;; To test perform a (clear), (load ...), and (testit)
;;; Note that a (clear) must be performed before rerunning
;;; this test because the initial when-defined salience values
;;; will have been lost.
;;;************************************************************

(deffunction testit ()
  (unwatch all)
  (set-strategy depth)
  (set-salience-evaluation when-defined)
  (printout t "Testing When Defined Salience Evaluation..." crlf)
  (reset)
  (run)
  (set-salience-evaluation when-activated)
  (printout t "Testing When Activated Salience Evaluation..." crlf)
  (reset)
  (run)
  (set-salience-evaluation every-cycle)
  (printout t "Testing Every Cycle Salience Evaluation..." crlf)
  (reset)
  (run)
  (set-salience-evaluation when-defined))

(deffunction print-order ($?x)
  (bind ?s (get-salience-evaluation))
  (if (eq ?s when-defined) then (bind ?r (nth 1 ?x)))
  (if (eq ?s when-activated) then (bind ?r (nth 2 ?x)))
  (if (eq ?s every-cycle) then (bind ?r (nth 3 ?x)))
  (printout t "This rule should fire " ?r crlf))

(defglobal ?*x* = 0)

;;;**************************
;;; RULE 1 - EVALUATION ORDER
;;; when-defined   - 1st  
;;; when-activated - 5th
;;; every-cycle    - 2nd
;;;**************************
(defrule rule-1 ""
   (declare (salience (+ ?*x* 6)))
   =>
   (bind ?*x* (+ ?*x* 4))
   (assert (activate-rule-4))
   (print-order 1st 1st 1st))

;;;**************************
;;; RULE 2 - EVALUATION ORDER
;;; when-defined   - 2nd  
;;; when-activated - 3rd
;;; every-cycle    - 6th
;;;**************************
(defrule rule-2 ""
   (declare (salience 5))
   =>
   (print-order 2nd 3rd 6th))

;;;**************************
;;; RULE 3 - EVALUATION ORDER
;;; when-defined   - 3rd  
;;; when-activated - 4th
;;; every-cycle    - 2nd
;;;**************************
(defrule rule-3 ""
   (declare (salience (+ ?*x* 4)))
   =>
   (bind ?*x* (+ ?*x* 4))
   (assert (activate-rule-7))
   (print-order 3rd 4th 2nd))

;;;**************************
;;; RULE 4 - EVALUATION ORDER
;;; when-defined   - 4th  
;;; when-activated - 2nd
;;; every-cycle    - 3rd
;;;**************************
(defrule rule-4 ""
   (declare (salience (+ ?*x* 3)))
   (activate-rule-4)
   =>
   (print-order 4th 2nd 3rd))

;;;**************************
;;; RULE 5 - EVALUATION ORDER
;;; when-defined   - 5th  
;;; when-activated - 6th
;;; every-cycle    - 7th
;;;**************************
(defrule rule-5 ""
   (declare (salience 2))
   =>
   (print-order 5th 6th 7th))

;;;**************************
;;; RULE 6 - EVALUATION ORDER
;;; when-defined   - 6th  
;;; when-activated - 7th
;;; every-cycle    - 4th
;;;**************************
(defrule rule-6 ""
   (declare (salience (+ ?*x* 1)))
   =>
   (print-order 6th 7th 4th))

;;;**************************
;;; RULE 7 - EVALUATION ORDER
;;; when-defined   - 7th  
;;; when-activated - 5th
;;; every-cycle    - 5th
;;;**************************
(defrule rule-7 ""
   (declare (salience ?*x*))
   (activate-rule-7)
   =>
   (print-order 7th 5th 5th))

