
(defglobal ?*which-one* = 1)

(deffunction retract-many ($?x)
  (bind ?i 1)
  (bind ?f (nth ?i ?x))
  (while (neq ?f nil) do
     (retract ?f)
     (bind ?i (+ 1 ?i))
     (bind ?f (nth ?i ?x))
     (if (neq ?f nil) then (printout t "..." crlf))))

(deffunction test-1 (?x)
  (reset)
  (assert (a))
  (bind ?*which-one* ?x)
  (run))

(deffunction test-2 (?x)
  (reset)
  (assert (g) (h) (i) (j) (k))
  (if (eq ?x 2) then (assert (l)))
  (if (eq ?x 3) then (assert (m)))
  (bind ?*which-one* ?x)
  (run))

(deffunction test-logical ()
  (printout t "TEST 1" crlf)
  (test-1 1)
  (test-1 2)
  (test-1 3)
  (test-1 4)
  (test-1 5)
  (printout t "TEST 2" crlf)
  (test-2 1)
  (test-2 2)
  (test-2 3))

(defrule logical1
  (a)
  =>
  (assert (b)))

(defrule logical2
  (logical (b))
  =>
  (assert (c)))

(defrule logical3
  (logical (c))
  =>
  (assert (d)))

(defrule logical4
  (logical (d))
  =>
  (assert (e)))

(defrule logical5
  (e)
  =>
  (assert (f)))

(defrule logical6
  ?f1 <- (a)
  ?f2 <- (b)
  ?f3 <- (c)
  ?f4 <- (d)
  ?f5 <- (e)
  ?f6 <- (f)
  =>
  (printout t "Trial " ?*which-one* crlf)
  (watch facts)
  (if (eq ?*which-one* 1) then (retract-many ?f1 ?f2))
  (if (eq ?*which-one* 2) then (retract-many ?f3 ?f4 ?f1 ?f2))
  (if (eq ?*which-one* 3) then (retract-many ?f4 ?f2 ?f1 ?f6))
  (if (eq ?*which-one* 4) then (retract-many ?f5 ?f4 ?f3 ?f2))
  (if (eq ?*which-one* 5) then (retract-many ?f6 ?f5 ?f2))
  (unwatch facts))

(defrule logical7
  (declare (salience 1))
  (logical (g))
  =>
  (assert (x)))

(defrule logical8
  (declare (salience 2))
  (logical (h))
  =>
  (assert (x)))

(defrule logical9
  (declare (salience 3))
  (logical (i))
  =>
  (assert (x)))

(defrule logical10
  (declare (salience 4))
  (or (logical (j))
      (logical (k)))
  =>
  (assert (x)))

(defrule logical11
  (declare (salience 5))
  (or (logical (g)) (logical (j)))
  =>
  (assert (x)))

(defrule logical12
  (declare (salience 6))
  (logical (i)) (h)
  =>
  (assert (x)))

(defrule logical13
  (declare (salience 7))
  (logical (i)) (h)
  =>
  (assert (x)))

(defrule logical14
  (declare (salience 8))
  (or (logical (j))
      (logical (k)))
  (or (logical (h))
      (logical (i))
      (logical (j)))
  =>
  (assert (x)))

(defrule logical15
  (declare (salience 9))
  (or (logical (g)) (logical (h)))
  (logical (k))
  =>
  (assert (x)))

(defrule logical16
  (declare (salience 10))
  (l)
  =>
  (assert (x)))

(defrule logical17
  (declare (salience -5))
  (m)
  =>
  (assert (x)))

(defrule logical18
  (declare (salience -10))
  ?f1 <- (g)
  ?f2 <- (h)
  ?f3 <- (i)
  ?f4 <- (j)
  ?f5 <- (k)
  =>
  (printout t "Trial " ?*which-one* crlf)
  (watch facts)
  (if (eq ?*which-one* 1) 
     then (retract-many ?f1 ?f2 ?f3 ?f4 ?f5))
  (if (eq ?*which-one* 2) 
     then (retract-many ?f5 ?f4 ?f3 ?f2 ?f1))
  (if (eq ?*which-one* 3) 
     then (retract-many ?f3 ?f4 ?f5 ?f1 ?f2))
  (unwatch facts))

