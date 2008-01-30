(clear) ; Test Begin   
(defglobal ?*constraint-salience* = 0)
(defrule bar 
  (declare (salience ?*constraint-salience*))
  ?f <- (x ?y&?x) 
  => 
  (retract ?ins))
(clear) ; Test Begin
(defrule foo
  (declare (salience ?*my-salience*)) =>)
(clear) ; Test Begin
(deftemplate status 
   (field parent))
(deffacts initial-positions
  (status (parent nil)))
(defrule move-alone 
  ?node <- (status (parent nil))
  =>
  (duplicate ?node (parent ?node)))
(clear) ; Test Begin
(defclass A (is-a USER) (role concrete) (pattern-match reactive))
(deffacts bar (a))
(definstances bar (a of A))
(defrule foo1 
  (logical (a))
  => 
  (assert (b)))
(defrule foo2 
  (logical (a))
  => 
  (make-instance [b] of A))
(defrule foo3 
  (logical (object (name [a])))
  => 
  (assert (c)))
(defrule foo4 
  (logical (object (name [a])))
  => 
  (make-instance [c] of A))
(reset)
(run)
(dependents 1)
(dependents 2)
(dependents 3)
(dependents [a])
(dependents [b])
(dependents [c])
(dependencies 1)
(dependencies 2)
(dependencies 3)
(dependencies [a])
(dependencies [b])
(dependencies [c])
(clear) ; Test Begin
(deftemplate yak (multifield x (type SYMBOL) (cardinality 3 5)))
(assert (yak (x a b c)))
(assert (yak (x a b 1)))
(assert (yak (x 1 b 1)))
(assert (yak (x a b)))
(assert (yak (x a b c d e f)))
(assert (yak (x a b c d e f g 1)))
(clear) ; Test Begin
(defrule foo (x ? ? ? ? ?) =>)
(defrule bar (x ? ? ? ? ?x&:(> ?x 3)) =>)
(deffacts yak (x a a a a abc))
(reset)
(clear)
(defrule foo (x ?) =>)
(defrule bar (x ?x&:(> ?x 3)) =>)
(deffacts yak (x abc))
(reset)
(clear)
(deftemplate x (field q) (field r) (field s) (field t))
(defrule foo (x (t ?)) =>)
(defrule bar (x (t ?x&:(> ?x 3))) =>)
(deffacts yak (x (t abc)))
(reset)
(clear) ; Test Begin
(deffunction positive-slope
   (?x1 ?y1 ?x2 ?y2)
   (< 0 (/ (- ?y2 ?y1) (- ?x2 ?x1))))
(defrule example-2
   (point ?a ?x1 ?y1)
   (point ?b ?x2 ?y2)
   (test (> ?b ?a))
   (test (positive-slope ?x1 ?y1 ?x2 ?y2))
   =>)
(assert (point 1 4.0 7.0) (point 2 5.0 9.0))
(clear) ; Test Begin
(deftemplate A (field foo (type INTEGER)))
(defrule foo (A (foo ?y&:(< ?y 3))) =>)
(clear) ; Test Begin
(deftemplate A
   (field grid-x)
   (field max)
   (field min-xy)
   (field max-xy))
(defrule p327
  (A (min-xy ?min) (max-xy ?max))
  (A (grid-x ?gx&:(and (>= ?gx ?min) (> ?gx ?max))))
  (A (max ?hmax&:(and (>= ?hmax ?gx) (> ?hmax ?min))))
  =>)
(clear) ; Test Begin
(deftemplate foo
   (field x (type INTEGER))
   (field y (type STRING))
   (field z (type FLOAT)))
(deftemplate bar
   (field x (type INTEGER))
   (field y (type STRING))
   (field z (type FLOAT)))
(defrule bad-1 (foo (x ?x) (y ?x)) =>)
(defrule bad-2 (foo (x ?x)) (bar (y ?x)) =>)
(defrule bad-3 (foo (x ?x) (y ?y)) (bar (z ?x | ?y)) =>)
(clear) ; Test Begin
(defrule fin
  (foo ?d)
  (test (> ?d 2))
  (foo ?l&~?d)
  =>)
(clear) ; Test Begin
(deftemplate foo1 (field x (cardinality 3 4)))
(deftemplate foo2 (multifield x (cardinality 3.0 4)))
(deftemplate foo3 (multifield x (cardinality 6 4)))
(clear) ; Test Begin
(defrule foo 
  (blah $?x ?y&red $?z)
  =>
  (printout t  ?x " | " ?y " | " ?z crlf))
(defrule bar 
  (yech $?x $?y)
  =>
  (printout t ?x " | " ?y crlf))
(assert (blah red) (blah red red) (blah a red b) (blah a red red b))
(assert (yech) (yech 1 2) (yech 1 2 3))
(run)
(clear)
