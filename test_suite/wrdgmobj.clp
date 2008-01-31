
;;;======================================================
;;;   Number Puzzle Problem
;;;     
;;;     Solves the number puzzle problem in which
;;;
;;;          GERALD 
;;;        + DONALD
;;;          ------
;;;        = ROBERT
;;;
;;;     CLIPS Version 6.0 Object Example
;;;
;;;     To execute, merely load, reset and run.
;;;     This example takes alot of memory to execute.
;;;======================================================

(defclass combination (is-a USER)
   (role concrete)
   (pattern-match reactive)
   (slot number (create-accessor write))
   (slot letter (create-accessor write)))

(defclass number (is-a USER)
   (role concrete)
   (pattern-match reactive)
   (slot value (create-accessor write)))

(defclass letter (is-a USER)
   (role concrete)
   (pattern-match reactive)
   (slot value (create-accessor write)))

(defrule startup
  =>
  (printout t crlf "The problem is" crlf crlf)
  (printout t "   GERALD" crlf)
  (printout t " + DONALD" crlf)
  (printout t "   ------" crlf)
  (printout t " = ROBERT" crlf crlf)
  (make-instance of number (value 0))
  (make-instance of number (value 1))
  (make-instance of number (value 2))
  (make-instance of number (value 3))
  (make-instance of number (value 4))
  (make-instance of number (value 5))
  (make-instance of number (value 6))
  (make-instance of number (value 7))
  (make-instance of number (value 8))
  (make-instance of number (value 9))
  (make-instance of letter (value G))
  (make-instance of letter (value E))
  (make-instance of letter (value R))
  (make-instance of letter (value A))
  (make-instance of letter (value L))
  (make-instance of letter (value D))
  (make-instance of letter (value O))
  (make-instance of letter (value N))
  (make-instance of letter (value B))
  (make-instance of letter (value T)))

(defrule generate-combinations
  (object (is-a number) (value ?x))
  (object (is-a letter) (value ?a))
  =>
  (make-instance of combination (letter ?a) (number ?x)))

(defrule find-solution
  (object (letter D) (number ?d))
  (object (letter T) (number ?t&~?d))
  (test (= (mod (+ ?d ?d) 10) ?t))
  (object (letter L) (number ?l&~?d&~?t))
  (object (letter R) (number ?r&~?d&~?t&~?l))
  (test (= (mod (+ ?d ?d
                   (* 10 ?l) (* 10 ?l))
                100)
           (+ (* 10 ?r) ?t)))
  (object (letter A) (number ?a&~?d&~?t&~?l&~?r))
  (object (letter E) (number ?e&~?d&~?t&~?l&~?r&~?a))
  (test (= (mod (+ ?d ?d
                   (* 10 ?l) (* 10 ?l)
                   (* 100 ?a) (* 100 ?a))
                1000)
           (+ (* 100 ?e) (* 10 ?r) ?t)))
  (object (letter N) (number ?n&~?d&~?t&~?l&~?r&~?a&~?e))
  (object (letter B) (number ?b&~?d&~?t&~?l&~?r&~?a&~?e&~?n))
  (test (= (mod (+ ?d ?d
                   (* 10 ?l) (* 10 ?l)
                   (* 100 ?a) (* 100 ?a)
                   (* 1000 ?r) (* 1000 ?n))
                10000)
           (+ (* 1000 ?b) (* 100 ?e) (* 10 ?r) ?t)))
  (object (letter O) (number ?o&~?d&~?t&~?l&~?r&~?a&~?e&~?n&~?b))
  (object (letter G) (number ?g&~?d&~?t&~?l&~?r&~?a&~?e&~?n&~?b&~?o))
  (test (= (+ ?d ?d
              (* 10 ?l) (* 10 ?l)
              (* 100 ?a) (* 100 ?a)
              (* 1000 ?r) (* 1000 ?n)
              (* 10000 ?e) (* 10000 ?o)
              (* 100000 ?g) (* 100000 ?d))
           (+ (* 100000 ?r) (* 10000 ?o) (* 1000 ?b) (* 100 ?e) (* 10 ?r) ?t)))
  =>
  (printout t "A Solution is:" crlf crlf)
  (printout t "  G = " ?g crlf)
  (printout t "  E = " ?e crlf)
  (printout t "  R = " ?r crlf)
  (printout t "  A = " ?a crlf)
  (printout t "  L = " ?l crlf)
  (printout t "  D = " ?d crlf)
  (printout t "  O = " ?o crlf)
  (printout t "  N = " ?n crlf)
  (printout t "  B = " ?b crlf)
  (printout t "  T = " ?t crlf)
  (printout t crlf)
  (printout t "   " ?g ?e ?r ?a ?l ?d crlf)
  (printout t " + " ?d ?o ?n ?a ?l ?d crlf) 
  (printout t "   " "------" crlf)
  (printout t " = " ?r ?o ?b ?e ?r ?t crlf crlf))  
  
 
