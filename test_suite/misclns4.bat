(clear) ;; Case 1
(defmodule A (export ?ALL))
(deftemplate A::foo)
(defmodule MAIN (import A ?ALL) (export ?ALL))
(defmodule B (import MAIN ?ALL))
(deftemplate B::foo)
(clear) ;; Case 2
(defmodule B)
(defmodule B::foo)
(clear) ;; Case 3
(defmodule ROOT (export ?ALL))
(deftemplate A)
(defmodule CHILD-1 (import ROOT ?ALL) (export ?ALL))
(defmodule CHILD-2 (import ROOT ?ALL) (export ?ALL))
(defmodule CHILD-3 (import CHILD-1 ?ALL) (import CHILD-2 ?ALL))
(clear) ;; Case 4
(defmodule d3 (export ?ALL))
(deftemplate t1)
(deftemplate t2)
(defmodule d4 (export deftemplate t3 t4) (import d3 deftemplate t1))
(clear) ;; Case 5
(deftemplate foo (slot x) (slot y))
(defrule bar (foo (x 3) (x 4) (y 3)) =>)
(clear) ;; Case 6
(deftemplate foo (slot x (type SYMBOL)))
(defrule bar (foo (x ?x)) => (+ ?x 1))
(defrule bar (foo (x ?x)) => (assert (yak (+ ?x 1))))
(clear) ;; Case 7
(defmodule a)
(assert (start))
(defrule foo (start) =>)
(defmodule b)
(assert (begin))
(defrule bar (begin) =>)
(agenda)
(agenda a)
(agenda b)
(agenda *)
(clear) ;; Case 8
(defglobal ?*x* = 0)
(defrule foo ?f <- (foo) => (bind ?*x* ?f))
(assert (foo))
(run)
(fact-index ?*x*)
(retract ?*x*)
(fact-index ?*x*)
(retract ?*x*)
(clear) ;; Case 9
(defmodule FOO (export ?ALL))
(deftemplate FOO::foo)
(defmodule BAR (export ?ALL) (import FOO ?ALL))
(deftemplate BAR::foo)
(clear) ;; Case 10
(defmodule FOO (export ?ALL))
(deftemplate FOO::foo)
(defmodule BAR (export ?ALL))
(deftemplate BAR::foo)
(defmodule YAK (import FOO ?ALL) (import BAR ?ALL))
(clear) ;; Case 11

(deftemplate set 
  (multislot members))

(deffacts stuff
  (set (members Q R S D T A B D)))

(defrule remove-duplicates
   ?f <- (set (members $?b ?element $?m ?element $?e))
   =>
   (printout t ?b " " ?element " " ?m " " ?e crlf)
   (modify ?f (members ?b ?element ?m ?e)))
(reset)
(run)
(facts)
(clear) ;; Case 12

(deftemplate set 
  (multislot m1)
  (multislot m2))

(deffacts stuff
  (set (m1 Q R S D T A B D X) (m2 Q R D A B D S X)))

(defrule remove-duplicates
   ?f <- (set (m1 $?b1 ?element1 $?m1 ?element1 $?e1 X)
              (m2 $?b2 ?element2 $?m2 ?element2 $?e2 X))
   =>
   (printout t ?b1 " " ?element1 " " ?m1 " " ?e1 crlf)
   (printout t ?b2 " " ?element2 " " ?m2 " " ?e2 crlf)
   (modify ?f (m1 ?b1 ?element1 ?m1 ?e1)
              (m2 ?b2 ?element2 ?m2 ?e2)))
(reset)
(run)
(facts)
(clear) ;; Case 13

(deftemplate set 
  (multislot m2))

(deffacts stuff
  (set (m2 D D S X)))

(defrule remove-duplicates
   ?f <- (set (m2 ?l2 $?m2 ?l2 $?e2 X))
   =>
   (printout t ?l2 " " ?m2 " " ?e2 crlf)
   (modify ?f (m2 ?l2 ?m2 ?e2)))
(reset)
(run)
(facts)
(clear) ;; Case 14
(defglobal ?*x* = 0)
(defmodule  MAIN (export ?ALL))
(defmodule FOO (import MAIN defglobal ?ALL))
(bind ?*x* 1)
?*x*
(clear) ;; Case 15

(defrule blah (m2 ?l2 $?m2 ?l2 $?e2 X) 
   => 
   (printout t ?l2 " " ?m2 " " ?e2 crlf))
(assert (m2 D D S X))
(run)
(clear) ;; Case 16

(deftemplate adder
  (multislot #-1)
  (multislot #-2))

(deffacts adder-info
  (adder (#-1 1 0 1) (#-2 1 1))
  (adder (#-1 1) (#-2 0)))

(defrule another-bug
  (adder (#-1 $?n1 ?v)
         (#-2 $?n2 ~?v))
  =>)
(reset)
(agenda)
(clear) ;; Case 17
(defmodule MAIN (export ?ALL))
(defmodule A (import MAIN ?ALL) (export ?ALL))
(deftemplate A::a (slot x))
(defmodule B (import MAIN ?ALL) (import A ?ALL) (export ?ALL))
(deftemplate B::b (slot y))
(clear) ;; Case 18

(defrule foo
  (declare (salience 4))
  (x)
  (not (and (a) (b)))
  (y)
  =>)
  
(defrule bar
  (declare (salience 3))
  (x)
  (not (and (a) (b)))
  (test (< 5 3))
  (y)
  =>)
  
(defrule dog
  (declare (salience 2))
  (x)
  (z)
  (not (and (a) (b)))
  (test (< 5 3))
  (y)
  =>)
  
(defrule yak
  (declare (salience 1))
  (x)
  (z)
  (not (and (a) (b)))
  (y)
  =>)
(reset)
(agenda)
(assert (x))
(agenda)
(assert (y))
(agenda)
(assert (z))
(agenda)
(assert (a))
(agenda)
(assert (b))
(agenda)
(clear) ;; Case 19
(assert (foo))
(defmodule BAR)
(assert (bar))
(save-facts "Temp//bug.tmp")
(bsave "Temp//bug.bin")
(bload "Temp//bug.bin")
(load-facts "Temp//bug.tmp") 
(facts)
(clear) ;; Case 20

(defrule foo1
  (declare (salience 10))
  =>)

(defrule foo2
  (declare (auto-focus TRUE))
  =>)

(defrule foo3
  (declare (auto-focus TRUE) (salience 10))
  =>)

(defrule foo4
  (declare (salience 10) (auto-focus TRUE))
  =>)
(ppdefrule foo1)
(ppdefrule foo2)
(ppdefrule foo3)
(ppdefrule foo4)
(clear) ; watching specific items
(unwatch all)
(defglobal ?*foo* = 1)
(defglobal ?*bar* = 2)
(deftemplate foo)
(deftemplate bar)
(defrule foo (foo) => (assert (bar)))
(defrule bar (bar) =>)
(watch rules foo)
(watch globals foo)
(watch facts foo)
(watch activations foo)
(watch rules yak)
(watch globals yak)
(watch facts yak)
(watch activations yak)
(watch compilations foo)
(watch statistics foo)
(watch focus foo)
(reset)
(bind ?*foo* 3)
(bind ?*bar* 4)
(assert (foo))
(run)
(list-watch-items)
(list-watch-items globals)
(list-watch-items facts)
(list-watch-items rules)
(list-watch-items activations)
(list-watch-items compilations)
(list-watch-items statistics)
(list-watch-items focus)
(unwatch all)
(list-watch-items)
(list-watch-items globals)
(list-watch-items facts)
(list-watch-items rules)
(list-watch-items activations)
(list-watch-items compilations)
(list-watch-items statistics)
(list-watch-items focus)
