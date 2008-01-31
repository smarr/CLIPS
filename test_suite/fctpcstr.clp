(deftemplate foo 
   (field x (type SYMBOL))
   (field y (type INTEGER)))

(deftemplate bar 
   (field a (type SYMBOL INTEGER))
   (field b (type INTEGER FLOAT))
   (field c (type SYMBOL STRING))) 

(defrule error-1 "Caught by CRSV, but only warning"
  (foo (x ?x) (y ?x))
  =>)

(defrule error-2 "Caught by CRSV, but only warning"
  (foo (x ?x))
  (foo (y ?x))
  =>)

(defrule error-3 "Caught by CRSV, but only warning"
  (bar (a ?x) (b ?x) (c ?x))
  =>)

(defrule error-4 "Caught by CRSV, but only warning"
  (bar (a ?x))
  (bar (b ?x))
  (bar (c ?x))
  =>)
    
(defrule okay!-1 
  (foo (x ?x) (y ?y))
  (bar (a ~?x&~?y))
  =>)

;;;
;;; Some Comments...
;;;

(deftemplate yak 
   (field a (type INTEGER) (range 4 7)))

(deftemplate obo 
   (field a (type INTEGER) (range 11 14)))

(deftemplate lok 
   (field a (type INTEGER) (range 6 12)))
   
(deftemplate vod 
   (field a (type INTEGER) (range 8 10)))

(defrule error-5 "Range Violation: Caught by CRSV"
  (yak (a ?x))
  (obo (a ?x))
  =>)

(defrule error-6 "Range Violation: Caught by CRSV"
  (yak (a ?x))
  (lok (a ?x))
  (obo (a ?x))
  =>)
  
(defrule error-7 "Range Violation: Caught by CRSV"
  (yak (a ?x))
  (obo (a ?y))
  (vod (a ?x | ?y))
  =>)

(defrule okay!-2 
  (yak (a ?x))
  (obo (a ?y))
  (lok (a ?x | ?y))
  =>)
  
;;; Some Comments 

(deftemplate kji (field x (type SYMBOL) (allowed-symbols on))
                 (field y (type SYMBOL) (allowed-symbols on off)))
               
(defrule error-8 "CRSV gives only 1 possible value warning--should be error"
  (kji (x ?x) (y ?x&~on))
  =>)

;;; 
;;; Some Comments
;;; 

(deftemplate foo7
  (field x (type SYMBOL FLOAT INTEGER)
           (allowed-symbols d e f)))

(deftemplate bar7
  (field x (type SYMBOL FLOAT INTEGER)
           (allowed-symbols a b c)))

(deftemplate yak7
  (field x (type SYMBOL)
           (allowed-symbols c d e)))

(defrule okay!-3 "CRSV gives error when SYMBOL/FLOAT fields can match"
  (foo7 (x ?x))
  (bar7 (x ?x))
  =>)
  
(defrule error-9 "No possible values: Caught by CRSV"
  (foo7 (x ?x))
  (bar7 (x ?x))
  (yak7 (x ?x))
  =>)

;;;
;;; Some Comments...
;;;
 
(deftemplate xxx 
   (field a (type INTEGER) (range 6 12)))
   
(deftemplate yyy 
   (field a (type INTEGER) (range 10 16)))
    
(defrule okay!-4
  (xxx (a ?x))
  (yyy (a ?x)) 
  =>)
  
(defrule okay!-5 
  (xxx (a ?x)) 
  (yyy (a ?x)) 
  =>)

;;; Slot x has a minimum of 7 fields and slot q has
;;; a maximum of 5 fields. Binding a variable in both
;;; patterns causes a cardinality constraint violation.

(deftemplate xoo (multifield x (cardinality 7 ?VARIABLE)))

(deftemplate yxk (multifield q (cardinality ?VARIABLE 5)))

(defrule error-10 "CRSV does not give a warning for this one"
  (xoo (x $?x))
  (yxk (q $?x))
  =>)

;;; Slot z has a minimum of 7 fields and slots x and y
;;; a maximum together of 5 fields. A cardinality
;;; violation is caused because $?x and $?y can add
;;; up at most to be 5 fields when placed in the z slot. 

(deftemplate fow (multifield x (cardinality ?VARIABLE 3)))
(deftemplate bww (multifield y (cardinality ?VARIABLE 2)))
(deftemplate wak (multifield z (cardinality 7 ?VARIABLE)))

(defrule error-11 "CRSV does not give a warning for this one"
   (fow (x $?x))
   (bww (y $?y))
   (wak (z $?x $?y))
   =>)

;;; More cardinality checks... 

(deftemplate fll (multifield x (cardinality 2 ?VARIABLE)))
(deftemplate blr (multifield y (cardinality 1 ?VARIABLE)))
(deftemplate qlz (multifield z (cardinality 1 ?VARIABLE)))

(deftemplate ylk (multifield q (cardinality ?VARIABLE 5)))

(defrule error-12 "CRSV does not give a warning for this one"
  (fll (x $?x))
  (blr (y $?y))
  (qlz (z $?z))
  (ylk (q $?x ? $?y ? b $?z))
  =>)

(defrule error-13 "CRSV does not give a warning for this one"
  (fll (x ?))
  =>)

(defrule okay!-6
  (fll (x ? ?))
  =>)

;;;
;;; ORing of fields
;;;

(deftemplate mnj
   (field x (type SYMBOL))
   (field y (type FLOAT)))

(defrule okay!-7 "CRSV gives inappropriate warning"
  (mnj (x ?x) (y ?y))
  (mnj (x ?x | ?y) (y ?x | ?y))
  =>)
  
;;;
;;; Check Return Value Types
;;;

(deftemplate kbp
   (field x (type SYMBOL))
   (field y (type FLOAT)))
   
(defrule error-14 "CRSV gives inappropriate error--does not detect return value error"
   (kbp (y ?y) (x =(+ ?y 3)))
   =>)

;;;
;;; Check Types of Variables passed to LHS functions
;;;

(deftemplate zc8 (field x (type SYMBOL)) (field y))

(defrule error-15 "Type Violation to LHS function call: Not caught by CRSV"
   (zc8 (x ?x) 
        (y ?y&:(> ?y ?x)))
   =>)

(defrule error-16 "Type Violation to LHS function call: Not caught by CRSV"
   (zc8 (x ?x) 
        (y ?y))
   (test (> ?y ?x))
   =>)

;;;
;;; Some unbound variable errors (not really constraint errors)
;;;

(defrule error-17 "Unbound variable error: Caught by CRSV"
   (a) 
   (test (> ?x 3)) 
   =>)

(defrule error-18 "Unbound variable error: Not caught by CRSV"
   (a ?y&:(or (> ?x 3) (< ?x 5)))
   =>)

;;;
;;; Statically check function arguments on the RHS of a rule
;;;

(defrule error-19 "Wrong argument type: Not caught by CRSV"
  =>
  (+ 3 a))

;;;
;;; Statically check function arguments on the LHS of a rule
;;;

(defrule error-20 "Wrong argument type: Not caught by CRSV"
  (buffy =(+ 3 a))
  =>)

;;;
;;; Check for constant conflicts
;;;

(deftemplate hh4 (field x))

(defrule error-21 "CRSV gives warning about & -- Rule cannot be  matched"
  (buffy a&b)
  =>)

(defrule error-22 "CRSV does not detect this error"
  (buffy a&~a)
  =>)

(defrule okay!-8 
   (hh4 (x a&~b)) 
   =>)
   
;;;
;;; Check for RHS function conflicts
;;;

(deftemplate rty (field x (type SYMBOL)))

(defrule error-23 "Not detected by CRSV"
  (rty (x ?q))
  =>
  (+ 3 5 ?q))
  
(defrule okay!-9 
  (rty (x ?q))
  =>
  (+ 3 5 ?q)
  (bind ?q 3))
  
(defrule error-24 "Not detected by CRSV"
  (rty)
  =>
  (bind ?q (gensym))
  (+ 3 5 ?q))
  
(defrule okay!-10
  (rty)
  =>
  (bind ?q (gensym))
  (+ 3 5 ?q)
  (bind ?q (* 9 3)))
  
;;;
;;; Yet another test
;;;

(deftemplate jhj (field x) (field y (type SYMBOL)))

(defrule error-25
  (jhj (x ?x&2) (y ?x))
  =>)

;;;
;;; Constraint Propogation from Expressions
;;;

(deftemplate yuck (field foo (type SYMBOL)))

(defrule error-26
  (blah ?x&:(> ?x 5))
  (yuck (foo ?x))
  =>)
  
(defrule error-27
  (yuck (foo ?x))
  (test (> ?x 5))
  =>)

(defrule error-28
  (blah ?x)
  (test (> ?x 5))
  (yuck (foo ?x))
  =>)
