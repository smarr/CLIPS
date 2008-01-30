(defclass FOO (is-a USER) (role concrete) (pattern-match reactive) 
   (slot x (type SYMBOL) (create-accessor ?NONE))
   (slot y (type INTEGER)))

(defclass BAR (is-a USER) (role concrete) (pattern-match reactive) 
   (slot a (type SYMBOL INTEGER) (create-accessor ?NONE))
   (slot b (type INTEGER FLOAT) (create-accessor ?NONE))
   (slot c (type SYMBOL STRING) (create-accessor ?NONE))) 

(defrule error-1
  (object (is-a FOO) (x ?x) (y ?x))
  =>)

(defrule error-2
  (object (is-a FOO) (x ?x))
  (object (is-a FOO) (y ?x))
  =>)

(defrule error-3
  (object (is-a BAR) (a ?x) (b ?x) (c ?x))
  =>)

(defrule error-4
  (object (is-a BAR) (a ?x))
  (object (is-a BAR) (b ?x))
  (object (is-a BAR) (c ?x))
  =>)
    
(defrule okay!-1 
  (object (is-a FOO) (x ?x) (y ?y))
  (object (is-a BAR) (a ~?x&~?y))
  =>)

(defclass YAK (is-a USER) (role concrete) (pattern-match reactive)  
   (slot a (type INTEGER) (range 4 7) (create-accessor ?NONE)))

(defclass OBO (is-a USER) (role concrete) (pattern-match reactive)  
   (slot a (type INTEGER) (range 11 14) (create-accessor ?NONE)))

(defclass LOK (is-a USER) (role concrete) (pattern-match reactive)  
   (slot a (type INTEGER) (range 6 12) (create-accessor ?NONE)))
   
(defclass VOD (is-a USER) (role concrete) (pattern-match reactive) 
   (slot a (type INTEGER) (range 8 10) (create-accessor ?NONE)))

(defrule error-5
  (object (is-a YAK) (a ?x))
  (object (is-a OBO) (a ?x))
  =>)

(defrule error-6
  (object (is-a YAK) (a ?x))
  (object (is-a LOK) (a ?x))
  (object (is-a OBO) (a ?x))
  =>)
  
(defrule error-7
  (object (is-a YAK) (a ?x))
  (object (is-a OBO) (a ?y))
  (object (is-a VOD) (a ?x | ?y))
  =>)

(defrule okay!-2 
  (object (is-a YAK) (a ?x))
  (object (is-a OBO) (a ?y))
  (object (is-a LOK) (a ?x | ?y))
  =>)
  
(defclass KJI (is-a USER) (role concrete) (pattern-match reactive)
  (slot x (type SYMBOL) (allowed-symbols on) (create-accessor ?NONE))
  (slot y (type SYMBOL) (allowed-symbols on off) (create-accessor ?NONE)))
               
(defrule error-8
  (object (is-a KJI) (x ?x) (y ?x&~on))
  =>)

(defclass FOO7 (is-a USER) (role concrete) (pattern-match reactive)
  (slot x (type SYMBOL FLOAT INTEGER)
           (allowed-symbols d e f) (create-accessor ?NONE)))

(defclass BAR7 (is-a USER) (role concrete) (pattern-match reactive)
  (slot x (type SYMBOL FLOAT INTEGER)
           (allowed-symbols a b c) (create-accessor ?NONE)))

(defclass YAK7 (is-a USER) (role concrete) (pattern-match reactive)
  (slot x (type SYMBOL)
           (allowed-symbols c d e) (create-accessor ?NONE)))

(defrule okay!-3
  (object (is-a FOO7) (x ?x))
  (object (is-a BAR7) (x ?x))
  =>)
  
(defrule error-9
  (object (is-a FOO7) (x ?x))
  (object (is-a BAR7) (x ?x))
  (object (is-a YAK7) (x ?x))
  =>)

(defclass XXX (is-a USER) (role concrete) (pattern-match reactive)
   (slot a (type INTEGER) (range 6 12) (create-accessor ?NONE)))
   
(defclass YYY (is-a USER) (role concrete) (pattern-match reactive)
   (slot a (type INTEGER) (range 10 16) (create-accessor ?NONE)))
    
(defrule okay!-4
  (object (is-a XXX) (a ?x))
  (object (is-a YYY) (a ?x)) 
  =>)
  
(defrule okay!-5 
  (object (is-a XXX) (a ?x)) 
  (object (is-a YYY) (a ?x)) 
  =>)

;;; Slot x has a minimum of 7 fields and slot q has
;;; a maximum of 5 fields. Binding a variable in both
;;; patterns causes a cardinality constraint violation.

(defclass XOO (is-a USER) (role concrete) (pattern-match reactive)
  (multislot x (cardinality 7 ?VARIABLE) (create-accessor ?NONE)))

(defclass YXK (is-a USER) (role concrete) (pattern-match reactive)
  (multislot q (cardinality ?VARIABLE 5) (create-accessor ?NONE)))

(defrule error-10
  (object (is-a XOO) (x $?x))
  (object (is-a YXK) (q $?x))
  =>)

;;; Slot z has a minimum of 7 fields and slots x and y
;;; a maximum together of 5 fields. A cardinality
;;; violation is caused because $?x and $?y can add
;;; up at most to be 5 fields when placed in the z slot. 

(defclass FOW (is-a USER) (role concrete) (pattern-match reactive)
  (multislot x (cardinality ?VARIABLE 3) (create-accessor ?NONE)))
(defclass BWW (is-a USER) (role concrete) (pattern-match reactive)
  (multislot y (cardinality ?VARIABLE 2) (create-accessor ?NONE)))
(defclass WAK (is-a USER) (role concrete) (pattern-match reactive)
  (multislot z (cardinality 7 ?VARIABLE) (create-accessor ?NONE)))

(defrule error-11
   (object (is-a FOW) (x $?x))
   (object (is-a BWW) (y $?y))
   (object (is-a WAK) (z $?x $?y))
   =>)

;;; More cardinality checks... 

(defclass FLL (is-a USER) (role concrete) (pattern-match reactive)
  (multislot x (cardinality 2 ?VARIABLE) (create-accessor ?NONE)))
(defclass BLR (is-a USER) (role concrete) (pattern-match reactive)
  (multislot y (cardinality 1 ?VARIABLE) (create-accessor ?NONE)))
(defclass QLZ (is-a USER) (role concrete) (pattern-match reactive)
  (multislot z (cardinality 1 ?VARIABLE) (create-accessor ?NONE)))
(defclass YLK (is-a USER) (role concrete) (pattern-match reactive)
  (multislot q (cardinality ?VARIABLE 5) (create-accessor ?NONE)))

(defrule error-12
  (object (is-a FLL) (x $?x))
  (object (is-a BLR) (y $?y))
  (object (is-a QLZ) (z $?z))
  (object (is-a YLK) (q $?x ? $?y ? b $?z))
  =>)

(defrule error-13
  (object (is-a FLL) (x ?))
  =>)

(defrule okay!-6
  (object (is-a FLL) (x ? ?))
  =>)

;;;
;;; ORing of fields
;;;

(defclass MNJ (is-a USER) (role concrete) (pattern-match reactive)
   (slot x (type SYMBOL) (create-accessor ?NONE))
   (slot y (type FLOAT) (create-accessor ?NONE)))

(defrule okay!-7
  (object (is-a MNJ) (x ?x) (y ?y))
  (object (is-a MNJ) (x ?x | ?y) (y ?x | ?y))
  =>)
  
;;;
;;; Check Return Value Types
;;;

(defclass KBP (is-a USER) (role concrete) (pattern-match reactive)
   (slot x (type SYMBOL) (create-accessor ?NONE))
   (slot y (type FLOAT) (create-accessor ?NONE)))
   
(defrule error-14
   (object (is-a KBP) (y ?y) (x =(+ ?y 3)))
   =>)

;;;
;;; Check Types of Variables passed to LHS functions
;;;

(defclass ZC8 (is-a USER) (role concrete) (pattern-match reactive)
  (slot x (type SYMBOL) (create-accessor ?NONE))
  (slot y (create-accessor ?NONE)))

(defrule error-15 "Type Violation to LHS function call"
   (object (is-a ZC8) (x ?x) 
        (y ?y&:(> ?y ?x)))
   =>)

(defrule error-16 "Type Violation to LHS function call"
   (object (is-a ZC8) (x ?x) 
        (y ?y))
   (test (> ?y ?x))
   =>)
   
;;;
;;; Check for RHS function conflicts
;;;

(defclass RTY (is-a USER) (role concrete) (pattern-match reactive)
  (slot x (type SYMBOL) (create-accessor ?NONE)))

(defrule error-17
  (object (is-a RTY) (x ?q))
  =>
  (+ 3 5 ?q))
  
(defrule okay!-8 
  (object (is-a RTY) (x ?q))
  =>
  (+ 3 5 ?q)
  (bind ?q 3))
  
(defrule error-18
  (object (is-a RTY))
  =>
  (bind ?q (gensym))
  (+ 3 5 ?q))
  
(defrule okay!-9
  (object (is-a RTY))
  =>
  (bind ?q (gensym))
  (+ 3 5 ?q)
  (bind ?q (* 9 3)))
  
;;;
;;; Yet another test
;;;

(defclass JHJ (is-a USER) (role concrete) (pattern-match reactive)
   (slot x (create-accessor ?NONE))
   (slot y (type SYMBOL) (create-accessor ?NONE)))

(defrule error-19
  (object (is-a JHJ) (x ?x&2) (y ?x))
  =>)

;;;
;;; Constraint Propogation from Expressions
;;;

(defclass YUCK (is-a USER) (role concrete) (pattern-match reactive)
  (slot foo (type SYMBOL) (create-accessor ?NONE)))

(defrule error-20
  (blah ?x&:(> ?x 5))
  (object (is-a YUCK) (foo ?x))
  =>)
  
(defrule error-21
  (object (is-a YUCK) (foo ?x))
  (test (> ?x 5))
  =>)

(defrule error-22
  (blah ?x)
  (test (> ?x 5))
  (object (is-a YUCK) (foo ?x))
  =>)

(defclass BASE (is-a USER) (role abstract)
  (multislot foo
     (type INTEGER)
     (cardinality 3 4)
     (allowed-integers 1 2 3) (create-accessor ?NONE)))

(defclass DERIVE1 (is-a BASE)
  (single-slot foo (source composite)
            (propagation no-inherit) (create-accessor ?NONE)))

(defclass DERIVE2 (is-a DERIVE1)
  (slot foo (type SYMBOL) (source composite) (create-accessor ?NONE)))

(defclass DERIVE2 (is-a DERIVE1)
  (slot foo (type SYMBOL INTEGER) (source composite) (create-accessor ?NONE)))

(defclass DERIVE3 (is-a DERIVE2)
  (slot foo (type SYMBOL INTEGER) (source composite) (cardinality 5 6) (create-accessor ?NONE)))

