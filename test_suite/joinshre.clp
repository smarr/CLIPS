;;; A completely new rule, so all new joins.
(defrule rule-1-1 "+j+j+j+j+j+j"
  (declare (salience 20))
  (a-1)
  (not (and (b-1) (c-1)))
  (d-1)
  =>)
  
(defrule rule-1-2 "=j=j=j=j+j+j"
  (declare (salience 19))
  (a-1)
  (not (and (b-1) (c-1)))
  (e-1)
  =>)
  
;; Can't reshare a-1 because the prior next join is negated
(defrule rule-1-3 "=j+j+j+j+j"
  (declare (salience 18))
  (a-1)
  (b-1)
  (c-1)
  (e-1)
  =>)

;;; This rule can share the first three patterns with rule-1-3,
;;; but the forth pattern requires a new join.
(defrule rule-1-4 "=j=j=j+j+j"
  (declare (salience 17))
  (a-1)
  (b-1)
  (c-1)
  (f-1)
  =>)

;;; A completely new rule, so all new joins.
(defrule rule-2-1 "+j+j+j+j+j"
  (declare (salience 16))
  (a-2)
  (b-2)
  (c-2)
  (d-2)
  =>)
  
(defrule rule-2-2 "=j=j+j+j+j+j"
  (declare (salience 15))
  (a-2)
  (b-2)
  (not (and (c-2)
            (d-2)))
  =>)
  
(defrule rule-2-3 "=j=j=j=j+j+j"
  (declare (salience 14))
  (a-2)
  (b-2)
  (not (not (and (c-2)
                 (d-2))))
  =>)
  
;;; A completely new rule, so all new joins.
(defrule rule-3-1 "+j+j+j+j+j+j"
  (declare (salience 13))
  (a-3)
  (b-3)
  (not (not (and (c-3)
                 (d-3))))
  =>) 
  
;;; All but the final join from the right
;;; can be shared
(defrule rule-3-2 "=j=j=j=j+j+j"
  (declare (salience 12))
  (a-3)
  (b-3)
  (not (and (c-3)
            (d-3)))
  =>)  

;;; a-3 and b-3 joins can be shared
(defrule rule-3-3 "=j=j+j+j+j"
  (declare (salience 11))
  (a-3)
  (b-3)
  (c-3)
  (d-3)
  =>)
  
;;; A completely new rule, so all new joins.  
(defrule rule-4-1 "+j+j+j+j+j"
   (declare (salience 10))
   (a-4) 
   (not (and (b-4) 
             (c-4))) 
   =>)

(defrule rule-4-2 "=j=j=j=j+j"
   (declare (salience 9))
   (a-4) 
   (not (and (b-4) 
             (c-4))) 
   =>)

;;; A completely new rule, so all new joins.
(defrule rule-5-1 "+j+j+j+j+j"
   (declare (salience 8))
   (a-5)
   (b-5)
   (c-5)
   (d-5)
   =>)
   
;;; The last join can't be shared with rule-5-1 since a single join 
;;; can't activate more than one rule.
(defrule rule-5-2 "=j=j=j=j+j"
   (declare (salience 7))
   (a-5)
   (b-5)
   (c-5)
   (d-5)
   =>)

;;; This is subsumed by rule-5-1 and since the c-5 join from rule-5-1
;;; does not activate a rule, then all of rule-5-1's joins can be used.
(defrule rule-5-3 "=j=j=j"
   (declare (salience 6))
   (a-5)
   (b-5)
   (c-5)
   =>)

;;; This rule should be able to share all but the last join since it's
;;; 1st four patterns are identical to rule-5-1 and rule-5-2.
(defrule rule-5-4 "=j=j=j=j+j"
   (declare (salience 5))
   (a-5)
   (b-5)
   (c-5)
   (d-5)
   (e-5)
   =>)

;;; This rule has only its first pattern in common with preexisting
;;; rules, so this is the only join it can share.
(defrule rule-5-5 "=j+j+j"
   (declare (salience 4))
   (a-5)
   (f-5)
   (g-5)
   =>)
   
;;; This rule has its first two patterns in common with preexisting
;;; rules, so these can be shared.
(defrule rule-5-6 "=j=j+j+j+j"
   (declare (salience 3))
   (a-5)
   (b-5)
   (f-5)
   (g-5)
   =>)
   
;;; This rule can use the preexisting a-5 join since it isn't used
;;; to activate any other rules.
(defrule rule-5-7 "=j+j"
   (declare (salience 2))
   (a-5)
   =>)
   
;;; This rule can't use the preexisting a-5 join since rule-5-6
;;; is activated by this join and this rule also needs to be
;;; activated by its only join.
(defrule rule-5-8 "=j+j"
   (declare (salience 1))
   (a-5)
   =>)

;;; extra join for initial-fact pattern  
(defrule rule-5-9 "+j+j+j"
   (declare (salience 0))
   (not (a-5))
   =>)

(defrule rule-5-10 "=j+j+j"
   (declare (salience -1))
   (a-5)
   (not (b-5))
   =>)
   
;;; extra join for initial-fact pattern  
(defrule rule-5-11 "=j=j+j+j"
   (declare (salience -2))
   (not (a-5))
   (b-5)
   =>)

;;; extra join for initial-fact pattern  
(defrule rule-5-12 "=j=j+j+j+j"
   (declare (salience -3))
   (not (a-5))
   (not (b-5))
   (c-5)
   =>)
   
;;; +j=j+j+j+j+j

(defrule rule-6-1 "+j=j+j+j+j+j"
   (a-6)
   (not (and (a-6)
             (not (b-6))
             (not (c-6))))
   =>)

;;; =j=j=j=j=j+j

(defrule rule-6-2 "=j=j=j=j=j+j"
   (a-6)
   (not (and (a-6)
             (not (b-6))
             (not (c-6))))
   =>)

