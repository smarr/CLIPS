(clear)                              ; CR0222
(remove misclns2.tmp)
(remove misclns2.bin)
(load misclns2.tmp)
(bload "Temp//misclns2.bin")
(open "Temp//misclns2.tmp" misc2 "w")
(printout misc2 "(defrule foo =>)")
(close misc2)
(load "Temp//misclns2.tmp")
(bsave "Temp//misclns2.bin")
(bload "Temp//misclns2.tmp")
(bload "Temp//misclns2.bin")
(clear)
(remove "Temp//misclns2.tmp")
(open "Temp//misclns2.tmp" misc2 "w")
(printout misc2 "(defrule foo bar =>)")
(close misc2)
(load "Temp//misclns2.tmp")
(remove "Temp//misclns2.tmp")
(remove "Temp//misclns2.bin")
(clear)                              ; CR0218
(insert$ (create$ 1 2) 1000 3)
(clear)                              ; CR0181 & CR0213
(defclass FOO (is-a USER) (role concrete) (pattern-match reactive))
(defrule foo1 (declare (salience 5)) =>)
(defrule foo2 (declare (salience 4)) (not (b)) =>)
(defrule foo3 (declare (salience 3)) (test (> 5 3)) =>)
(defrule foo4 (declare (salience 2)) (test (> 5 3)) (not (b)) =>)
(defrule foo5 (declare (salience 1)) (not (object (is-a FOO))) =>)
(defrule foo6 (test (> 5 3)) (not (object (is-a FOO))) =>)
(reset)
(agenda)
(clear)                              ; Bug Test
(defrule foo1 (bar a $?x b) => (printout t "1: " ?x crlf))
(defrule foo2 (bar $?x a) => (printout t "2: " ?x crlf))
(defrule foo3 (bar a b $?x c d) => (printout t "3: " ?x crlf))
(assert (bar a b c d))
(assert (bar a b 1 2 3 c d))
(agenda)
(run)
(clear)                              ; Bug Test
(defrule foo1 (bar) =>)
(defrule foo2 (bar ?) =>)
(defrule foo3 (bar () =>))
(defrule foo4 (bar ~) =>)
(ppdefrule foo1)
(ppdefrule foo2)
(assert (bar))
(assert (bar 1))
(agenda)
(clear)
(deftemplate yak (multifield bar))
(defrule foo1 (yak (bar)) =>)
(defrule foo2 (yak (bar ?)) =>)
(defrule foo3 (yak (bar ()) =>))
(defrule foo4 (yak (bar ~)) =>)
(ppdefrule foo1)
(ppdefrule foo2)
(assert (yak (bar)))
(assert (yak (bar 1)))
(agenda)
(clear)                              ; Bug Test
(defrule foo (a $?x b) => (printout t ?x crlf))
(assert (a b))
(assert (a c b))
(agenda)
(run)
(clear)                              ; Bug Test
(defrule blah (fact $?x here $?x) => (printout t ?x crlf))
(assert (fact))
(assert (fact 1 here 2))
(assert (fact 1 2 here 3 4))
(assert (fact here))
(assert (fact 5 here 5))
(assert (fact 6 7 here 6 7))
(assert (fact 6 7 here 6 8))
(assert (fact 6 8 here 7 8))
(agenda)
(run)
(clear)                              ; Use of reserved symbols
(deftemplate and "example")
(deftemplate not)
(deftemplate or "example")
(deftemplate test)
(deftemplate logical "example" (field y))
(deftemplate exists)
(deftemplate forall (field x))
(deftemplate object "example")
(assert (and))
(assert (not b))
(defrule foo => (assert (or c)))
(assert (test))
(assert (logical))
(assert (this) (exists))
(assert (forall) (that))
(assert (object))
(clear)                              ; First pattern field usage
(defrule foo ($?x&abc) =>)
(clear)                              ; Order Dependency
(deftemplate foo (field x) (field y))
(defrule blah (foo (y ?y&:(numberp ?y)) (x =(+ ?y 1))) =>)
(assert (foo (y a) (x 3)))
(clear)                              ; Bind & RHS constraints
(deftemplate foo (field x (type SYMBOL)))

(defrule bar1 
  (foo (x ?x))
  =>
  (+ ?x 1))

(defrule bar2
  (foo (x ?x))
  =>
  (sym-cat ?x ?x))

(defrule bar3
  (foo (x ?x))
  =>
  (bind ?x 3)
  (sym-cat ?x ?x)
  (+ ?x 1))

(defrule bar4
  (foo (x ?x))
  =>
  (sym-cat ?x ?x)
  (+ ?x 1)
  (bind ?x 3))
(clear) ; Thing 1
(defrule foo
   ?f <- (bar)
   (test (neq ?f g))
   =>)
(clear) ; Thing 2
(deftemplate status 
   (field farmer)
   (field fox))
(defrule move-alone 
  (status (farmer ?fs))
  =>)
(defrule move-with-fox
  (status (farmer ?fs) (fox ?fs))
=>)
(deffacts stuff 
  (status (farmer 1) (fox 2))
  (status (farmer 3) (fox 3)))
(reset)
(agenda)
(clear) ; Thing 3
(load other1.clp)
(clear) ; Thing 4
(defrule foo
 "12345678901234567890123456789012345678901234567890
  12345678901234567890123456789012345678901234567890
  12345678901234567890123456789012345678901234567890
  12345678901234567890123456789012345678901234567890
  12345678901234567890123456789012345678901234567890
  12345678901234567890123456789012345678901234567890
  12345678901234567890123456789012345678901234567890
  12345678901234567890123456789012345678901234567890
  12345678901234567890123456789012345678901234567890
  12345678901234567890123456789012345678901234567890
  12345678901234567890123456789012345678901234567890
  12345678901234567890123456789012345678901234567890
  12345678901234567890123456789012345678901234567890
  12345678901234567890123456789012345678901234567890
  12345678901234567890123456789012345678901234567890
  12345678901234567890123456789012345678901234567890
  12345678901234567890123456789012345678901234567890"
  =>)
(clear) ; Thing 5
(deftemplate foo (field x) (field y))
(defrule bar (foo (y ?y&:(> ?y 3))) =>)
(assert (foo (y a)))
(defrule yak (quo ? ?x&:(> ?x 3)) =>)
(assert (quo b a))
(clear) ; Thing 6
(watch facts)
(reset)
(clear)
(clear) ; Thing 7
(deftemplate bar (slot y) (slot x) (slot z))
(defrule foo1 (bar (x ?x&:(> ?x 3))) =>)
(defrule foo2 (bar (y ?x&:(> ?x 3))) =>)
(defrule foo3 (bar (z ?x&:(> ?x 3))) =>)
(assert (bar (x a) (y 1) (z 1)))
(assert (bar (x 1) (y a) (z 1)))
(assert (bar (x 1) (y 1) (z a)))
(clear) ; Thing 8
(defrule foo1 (bar ?x&:(> ?x 3) ?) =>)
(defrule foo2 (bar ? ?x&:(> ?x 3)) =>)
(assert (bar a 1))
(assert (bar 1 a))
(clear) ; Can't delete deftemplates while they're being used
(deftemplate foo)
(undeftemplate foo)
(deftemplate foo)
(assert (foo))
(undeftemplate foo)
(defrule foobar => (assert (foo)))
(undeftemplate foo)
(retract 1)
(undeftemplate foo)
(undefrule foobar)
(undeftemplate foo)
(clear)
(deftemplate foo)
(defrule bar (foo) =>)
(deftemplate foo)
(clear)
(deftemplate foo)
(deftemplate foo (slot x))
(clear) ; Next Thing
