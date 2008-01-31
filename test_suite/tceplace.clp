;;;************************************************************
;;; TEST CE PLACEMENT CHECKING
;;;
;;; This file tests to see if the expressions generated for 
;;; test CEs are placed at the appropriate join in the join
;;; network when used in conjunction with joins from the right.
;;;************************************************************

(defrule foo1
  (declare (salience 9))
  (a)
  (not (and (not (and (b) (c)))
            (test (< 3 5))))
  (test (< 3 5))
  =>)

(defrule foo2
  (declare (salience 7))
  (d)
  (not (and (not (and (b) (c)))
            (test (> 3 5))))
  (test (< 3 5))
  =>)

(defrule foo3
  (e)
  (not (and (not (and (b) (c)))
            (test (> 3 5))))
  (test (> 3 5))
  =>)

(defrule foo4
  (f)
  (not (and (not (and (b) (c)))
            (test (< 3 5))))
  (test (> 3 5))
  =>)

(defrule foo5
  (declare (salience 5))
  (g)
  (not (not (and (b) (c) (test (< 3 5)))))
  (test (< 3 5))
  =>)

(defrule foo6
  (h)
  (not (not (and (b) (c) (test (> 3 5)))))
  (test (< 3 5))
  =>)

(defrule foo7
  (i)
  (not (not (and (b) (c) (test (> 3 5)))))
  (test (> 3 5))
  =>)

(defrule foo8
  (j)
  (not (not (and (b) (c) (test (< 3 5)))))
  (test (> 3 5))
  =>)

(defrule foo9
  (k)
  (not (and (b)))
  (test (< 5 3))
  =>)

(defrule foo10
  (declare (salience 3))
  (l)
  (not (and (b)
            (test (< 5 3))))
  =>)

