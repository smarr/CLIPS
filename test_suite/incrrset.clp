;;;*******************************************************************
;;; INCREMENTAL RESET ERROR CHECKING
;;;
;;; This file tests the incremental reset capability. It 
;;; specifically tests the following attributes:
;;;
;;;   (1) Negated patterns are initialized without performing a reset.
;;;   (2) Negated patterns are initialized after performing a reset.
;;;   (3) Negated and non-negated patterns are initialized without
;;;       performing a reset.
;;;   (4) Negated and non-negated patterns are initialized after
;;;       performing a reset.
;;;   (5) Non-initialized rules are initialized properly when
;;;       they share patterns and joins with initialized rules.
;;;
;;; To test perform a (clear), (load ...), and (testit)
;;; There are six tests. Each test should print out which test is
;;; being performed and how many rules should fire. The rules should
;;; then fire printing out the order of execution (e.g. 1st, 2nd, 3rd,
;;; etc.). If the stated number of rules does not fire, or the rules
;;; fire out of order, then a bug has been detected.
;;;*******************************************************************

(deffunction make-rule (?name ?salience ?RHS ?order)
  (build 
        (str-cat (format nil "(defrule %s " ?name)
                 (format nil "(declare (salience %d)) " ?salience)
                 (format nil "%s => " ?RHS)
                 (format nil "(printout t \"This rule should fire \" %s crlf))" ?order))))
                  
(deffunction testit ()
   (unwatch all)
   ; All negated patterns added after reset
   (undefrule *)
   (reset)
   (printout t "Test 1 - 5 Rules should fire..." crlf)
   (make-rule blah-1 95 "(not (a))" 1st)
   (make-rule blah-2 90 "(not (b))" 2nd)
   (make-rule blah-3 85 "(not (b)) (not (a))" 3rd)
   (make-rule blah-4 80 "(not (c)) (not (a))" 4th)
   (make-rule blah-5 75 "(not (a)) (not (d))" 5th)
   (run)
   ; All negated patterns added before reset
   (printout t "Test 2 - 5 Rules should fire..." crlf)
   (reset)
   (run)
   ; Mix of patterns added after reset
   (undefrule *)
   (reset)
   (assert (e) (f) (g))
   (printout t "Test 3 - 16 Rules should fire..." crlf)
   (make-rule blah-1 95 "(e) (not (a))" 1st)
   (make-rule blah-2 90 "(not (b)) (f)" 2nd)
   (make-rule blah-3 85 "(not (b)) (g) (not (a))" 3rd)
   (make-rule blah-4 80 "(f) (not (c)) (not (a)) (e)" 4th)
   (make-rule blah-5 75 "(e) (f) (not (a)) (g) (not (d))" 5th)
   (make-rule blah-6 70 "" 6th)
   (make-rule blah-7 65 "(e)" 7th)
   (make-rule blah-8 60 "(e) (f)" 8th)
   (make-rule blah-9 55 "(f) (e)" 9th)
   (make-rule blah-A 50 "(g) (f) (e)" 10th)
   (make-rule blah-B 45 "(f) (e)" 11th)
   (make-rule blah-C 40 "(e) (f) (not (a)) (g) (not (d))" 12th)
   (make-rule blah-D 35 "(not (b)) (g) (not (a))" 13th)
   (make-rule blah-E 30 "(f) (e) (g)" 14th)
   (make-rule blah-F 25 "(not (b)) (f) (not (b))" 15th)
   (make-rule blah-G 20 "(g) (not (c)) (g)" 16th)
   (run)
   ; Mix of patterns added before reset
   (printout t "Test 4 - 16 Rules should fire..." crlf)
   (reset)
   (assert (e) (f) (g))
   (run)
   ; Sharing of patterns added before and after reset
   (undefrule *)
   (reset)
   (make-rule blah-1 95 "(e) (f) (g)" 1st)
   (make-rule blah-2 90 "(not (b)) (g) (not (a))" 2nd)
   (make-rule blah-3 85 "(g) (not (c)) (g)" 3rd)
   (make-rule blah-4 80 "(not (a)) (not (b)) (not (c))" 4th)
   (assert (e) (f) (g) (h))
   (printout t "Test 5 - 23 Rules should fire..." crlf)
   (make-rule blah-5 75 "(e) (f) (g)" 5th)
   (make-rule blah-6 70 "(f)" 6th)
   (make-rule blah-7 65 "(g) (f) (e)" 7th)
   (make-rule blah-8 60 "(e) (h)" 8th)
   (make-rule blah-9 55 "(e) (f) (g) (h)" 9th)
   (make-rule blah-A 50 "(e) (f) (e) (f)" 10th)
   (make-rule blah-B 45 "(e) (f) (not (a))" 11th)
   (make-rule blah-C 40 "(e) (not (s)) (not (b))" 12th)
   (make-rule blah-D 35 "(not (b)) (g) (not (a))" 13th)
   (make-rule blah-E 30 "(not (b)) (g)" 14th)
   (make-rule blah-F 25 "(not (b)) (f) (e)" 15th)
   (make-rule blah-G 22 "(g) (not (c)) (h)" 16th)
   (make-rule blah-H 20 "(g) (not (c)) (not (a))" 17th)
   (make-rule blah-I 18 "(not (c)) (g) (not (a))" 18th)
   (make-rule blah-J 16 "(not (a)) (not (b)) (not (c))" 19th)
   (make-rule blah-K 14 "(not (c)) (not (a)) (not (b))" 20th)
   (make-rule blah-L 12 "(not (a)) (not (b)) (h)" 21st)
   (make-rule blah-M 11 "(not (a))" 22nd)
   (make-rule blah-N 10 "(e)" 23rd)
   (run)
   ; Sharing of patterns added before reset
   (printout t "Test 6 - 23 Rules should fire..." crlf)
   (reset)
   (assert (e) (f) (g) (h))
   (run)
   ; Sharing of joins from the right
   (undefrule *)
   (reset)
   (printout t "Test 7 - 0 Rules should fire..." crlf)
   (assert (a) (b) (c) (d))
   (make-rule blah-1 95 "(a) (not (and (b) (c)))" 1st)
   (make-rule blah-2 90 "(d) (not (and (b) (c)))" 2nd)
   (run)
   (undefrule *)
   )

