;;;************************************************************
;;; CONFLICT RESOLUTION ERROR CHECKING
;;;
;;; This file tests the following conflict resolution strategies
;;;   1) depth
;;;   2) breadth
;;;   3) simplicity
;;;   4) complexity
;;;   5) lex
;;;   6) mea
;;;
;;; The random strategy is not tested. Also lex and mea are
;;; not extensively tested.
;;; To test perform a (clear), (load ...), and (testit)
;;;************************************************************

(deffacts info
  (fact a)
  (fact b 1)
  (fact c 1 2)
  (fact d 1 2 3)
  (fact e 1 2 3 4)
  (fact f 1 2 3 4)
  (fact g 1 2 3) 
  (fact h 1 2)
  (fact i 1)
  (fact j))

(deffunction testit ()
  (unwatch all)
  (set-strategy depth)
  (printout t "Testing Depth Strategy..." crlf)
  (reset)
  ;(printout t "Running..." crlf)
  (run)
  (set-strategy breadth)
  (printout t "Testing Breadth Strategy..." crlf)
  (reset)
  (run)
  (set-strategy simplicity)
  (printout t "Testing Simplicity Strategy..." crlf)
  (reset)
  (run)
  (set-strategy complexity)
  (printout t "Testing Complexity Strategy..." crlf)
  (reset)
  (run)
  (set-strategy lex)
  (printout t "Testing LEX Strategy..." crlf)
  (reset)
  (run)
  (set-strategy mea)
  (printout t "Testing MEA Strategy..." crlf)
  (reset)
  (run)
  (set-strategy depth))

(deffunction print-order ($?x)
  (bind ?s (get-strategy))
  (if (eq ?s depth) then (bind ?r (nth 1 ?x)))
  (if (eq ?s breadth) then (bind ?r (nth 2 ?x)))
  (if (eq ?s simplicity) then (bind ?r (nth 3 ?x)))
  (if (eq ?s complexity) then (bind ?r (nth 4 ?x)))
  (if (eq ?s lex) then (bind ?r (nth 5 ?x)))
  (if (eq ?s mea) then (bind ?r (nth 6 ?x)))
  (printout t "This rule should fire " ?r crlf))

;;;************************
;;; RULE 1 - STRATEGY ORDER
;;; depth      - 1st  
;;; breadth    - 5th
;;; simplicity - 2nd
;;; complexity - 4th
;;; lex        - 1st 
;;; mea        - 5th 
;;;************************
(defrule rule-1 "complexity = 6"
   (declare (salience 10))
   (fact a&~j)
   (fact j&~b)
   =>
   (print-order 1st 5th 2nd 4th 1st 5th))

;;;************************
;;; RULE 2 - STRATEGY ORDER
;;; depth      - 5th  
;;; breadth    - 1st
;;; simplicity - 4th
;;; complexity - 2nd
;;; lex        - 5th  
;;; mea        - 4th 
;;;************************
(defrule rule-2  "complexity = 8"
   (declare (salience 10))
   (fact b ?x)
   (fact c ?x ?y)
   (fact d ?x ?z ?w)
   =>
   (print-order 5th 1st 4th 2nd 5th 4th))

;;;************************
;;; RULE 3 - STRATEGY ORDER
;;; depth      - 4th  
;;; breadth    - 2nd
;;; simplicity - 1st
;;; complexity - 5th
;;; lex        - 4th  
;;; mea        - 3rd 
;;;************************
(defrule rule-3 "complexity = 5"
   (declare (salience 10))
   (fact g 1 2 3)
   =>
   (print-order 4th 2nd 1st 5th 4th 3rd))

;;;************************
;;; RULE 4 - STRATEGY ORDER
;;; depth      - 3rd  
;;; breadth    - 3rd
;;; simplicity - 3rd
;;; complexity - 3rd
;;; lex        - 3rd  
;;; mea        - 2nd 
;;;************************
(defrule rule-4 "complexity = 7"
   (declare (salience 10))
   (fact h ? 2)
   (fact e ? ?x&:(= ?x 2) ? ?z&=(+ 2 2))
   =>
   (print-order 3rd 3rd 3rd 3rd 3rd 2nd))

;;;************************
;;; RULE 5 - STRATEGY ORDER
;;; depth      - 2nd  
;;; breadth    - 4th
;;; simplicity - 5th
;;; complexity - 1st
;;; lex        - 2nd  
;;; mea        - 1st
;;;************************
(defrule rule-5  "complexity = 9"
   (declare (salience 10))
   (fact i ?x)
   (test (eq ?x 1))
   (fact f ?y&:(= ?y (+ 2 -1)) ?z&:(and (< ?z 4) (= ?z 2)) 3 ?)
   =>
   (print-order 2nd 4th 5th 1st 2nd 1st))

;;;************************
;;; RULE 6 - STRATEGY ORDER
;;; depth      - 6th  
;;; breadth    - 10th
;;; simplicity - 7th
;;; complexity - 9th
;;; lex        - 6th 
;;; mea        - 10th 
;;;************************
(defrule rule-6 "complexity = 6"
   (declare (salience 0))
   (fact a&~j)
   (fact j&~b)
   =>
   (print-order 6th 10th 7th 9th 6th 10th))

;;;************************
;;; RULE 7 - STRATEGY ORDER
;;; depth      - 10th  
;;; breadth    - 6th
;;; simplicity - 9th
;;; complexity - 7th
;;; lex        - 10th  
;;; mea        - 9th 
;;;************************
(defrule rule-7  "complexity = 8"
   (declare (salience 0))
   (fact b ?x)
   (fact c ?x ?y)
   (fact d ?x ?z ?w)
   =>
   (print-order 10th 6th 9th 7th 10th 9th))

;;;************************
;;; RULE 8 - STRATEGY ORDER
;;; depth      - 9th  
;;; breadth    - 7th
;;; simplicity - 6th
;;; complexity - 10th
;;; lex        - 9th  
;;; mea        - 8th 
;;;************************
(defrule rule-8 "complexity = 5"
   (declare (salience 0))
   (fact g 1 2 3)
   =>
   (print-order 9th 7th 6th 10th 9th 8th))

;;;************************
;;; RULE 9 - STRATEGY ORDER
;;; depth      - 8th  
;;; breadth    - 8th
;;; simplicity - 8th
;;; complexity - 8th
;;; lex        - 8th  
;;; mea        - 7th 
;;;************************
(defrule rule-9 "complexity = 7"
   (declare (salience 0))
   (fact h ? 2)
   (fact e ? ?x&:(= ?x 2) ? ?z&=(+ 2 2))
   =>
   (print-order 8th 8th 8th 8th 8th 7th))

;;;************************
;;; RULE 10 - STRATEGY ORDER
;;; depth      - 7th  
;;; breadth    - 9th
;;; simplicity - 10th
;;; complexity - 6th
;;; lex        - 7th  
;;; mea        - 6th
;;;************************
(defrule rule-10  "complexity = 9"
   (declare (salience 0))
   (fact i ?x)
   (test (eq ?x 1))
   (fact f ?y&:(= ?y (+ 2 -1)) ?z&:(and (< ?z 4) (= ?z 2)) 3 ?)
   =>
   (print-order 7th 9th 10th 6th 7th 6th))

;;;************************
;;; RULE 11 - STRATEGY ORDER
;;; depth      - 11th  
;;; breadth    - 15th
;;; simplicity - 12th
;;; complexity - 14th
;;; lex        - 11th 
;;; mea        - 15th 
;;;************************
(defrule rule-11 "complexity = 6"
   (declare (salience -10))
   (fact a&~j)
   (fact j&~b)
   =>
   (print-order 11th 15th 12th 14th 11th 15th))

;;;************************
;;; RULE 12 - STRATEGY ORDER
;;; depth      - 15th  
;;; breadth    - 11th
;;; simplicity - 14th
;;; complexity - 12th
;;; lex        - 15th  
;;; mea        - 14th 
;;;************************
(defrule rule-12  "complexity = 8"
   (declare (salience -10))
   (fact b ?x)
   (fact c ?x ?y)
   (fact d ?x ?z ?w)
   =>
   (print-order 15th 11th 14th 12th 15th 14th))

;;;************************
;;; RULE 13 - STRATEGY ORDER
;;; depth      - 14th  
;;; breadth    - 12th
;;; simplicity - 11th
;;; complexity - 15th
;;; lex        - 14th  
;;; mea        - 13th 
;;;************************
(defrule rule-13 "complexity = 5"
   (declare (salience -10))
   (fact g 1 2 3)
   =>
   (print-order 14th 12th 11th 15th 14th 13th))

;;;************************
;;; RULE 14 - STRATEGY ORDER
;;; depth      - 13th  
;;; breadth    - 13th
;;; simplicity - 13th
;;; complexity - 13th
;;; lex        - 13th  
;;; mea        - 12th 
;;;************************
(defrule rule-14 "complexity = 7"
   (declare (salience -10))
   (fact h ? 2)
   (fact e ? ?x&:(= ?x 2) ? ?z&=(+ 2 2))
   =>
   (print-order 13th 13th 13th 13th 13th 12th))

;;;************************
;;; RULE 15 - STRATEGY ORDER
;;; depth      - 12th  
;;; breadth    - 14th
;;; simplicity - 15th
;;; complexity - 11th
;;; lex        - 12th  
;;; mea        - 11th
;;;************************
(defrule rule-15  "complexity = 9"
   (declare (salience -10))
   (fact i ?x)
   (test (eq ?x 1))
   (fact f ?y&:(= ?y (+ 2 -1)) ?z&:(and (< ?z 4) (= ?z 2)) 3 ?)
   =>
   (print-order 12th 14th 15th 11th 12th 11th))
