 
;;;======================================================
;;;   Farmer's Dilemma Problem
;;;
;;;     Another classic AI problem (cannibals and the 
;;;     missionary) in agricultural terms. The point is
;;;     to get the farmer, the fox the cabbage and the
;;;     goat across a stream.
;;;        But the boat only holds 2 items. If left 
;;;     alone with the goat, the fox will eat it. If
;;;     left alone with the cabbage, the goat will eat
;;;     it.
;;;        This example uses rules and object pattern  
;;;     matching to solve the problem.
;;;
;;;     CLIPS Version 6.0 Example
;;;
;;;     To execute, merely load, reset and run.
;;;======================================================

;;;***********
;;;* CLASSES *
;;;***********

;;; The status instances hold the state  
;;; information of the search tree.

(defclass status (is-a USER)
   (role concrete)
   (pattern-match reactive)
   (slot search-depth
     (create-accessor write)
     (type INTEGER) (range 1 ?VARIABLE) (default 1)) 
   (slot parent
     (create-accessor write)
     (type INSTANCE-ADDRESS) (default ?DERIVE))
   (slot farmer-location 
     (create-accessor write)
     (type SYMBOL) (allowed-symbols shore-1 shore-2) (default shore-1))
   (slot fox-location
     (create-accessor write)
     (type SYMBOL) (allowed-symbols shore-1 shore-2) (default shore-1))
   (slot goat-location
     (create-accessor write)
     (type SYMBOL) (allowed-symbols shore-1 shore-2) (default shore-1))
   (slot cabbage-location
     (create-accessor write)
     (type SYMBOL) (allowed-symbols shore-1 shore-2) (default shore-1))
   (slot last-move
     (create-accessor write)
     (type SYMBOL) (allowed-symbols no-move alone fox goat cabbage)
     (default no-move)))
   
;;; The moves instances hold the information of all the moves
;;; made to reach a given state.
       
(defclass moves (is-a USER)
   (role concrete)
   (pattern-match reactive)
   (slot id
      (create-accessor write)
      (type INSTANCE)) 
   (multislot moves-list 
      (create-accessor write)
      (type SYMBOL)
      (allowed-symbols no-move alone fox goat cabbage)))

(defclass opposite-of
   (is-a USER)
   (role concrete)
   (pattern-match reactive)
   (slot value (create-accessor write))
   (slot opposite-value (create-accessor write)))

;;;*****************
;;;* INITIAL STATE *
;;;*****************

(definstances startups
  (of status)
  (of opposite-of (value shore-1) (opposite-value shore-2))
  (of opposite-of (value shore-2) (opposite-value shore-1)))

;;;***********************
;;;* GENERATE PATH RULES *
;;;***********************

(defrule move-alone 
  ?node <- (object (is-a status)
                   (search-depth ?num)  
                   (farmer-location ?fs))
  (object (is-a opposite-of) (value ?fs) (opposite-value ?ns))
  =>
  (duplicate-instance ?node
    (search-depth (+ 1 ?num))
    (parent ?node)
    (farmer-location ?ns)
    (last-move alone)))

(defrule move-with-fox
  ?node <- (object (is-a status)
                   (search-depth ?num) 
                   (farmer-location ?fs)
                   (fox-location ?fs))
  (object (is-a opposite-of) (value ?fs) (opposite-value ?ns))
  =>
  (duplicate-instance ?node
    (search-depth (+ 1 ?num))
    (parent ?node)
    (farmer-location ?ns)
    (last-move fox)
    (fox-location ?ns)))

(defrule move-with-goat 
  ?node <- (object (is-a status)
                   (search-depth ?num) 
                   (farmer-location ?fs)
                   (goat-location ?fs))
  (object (is-a opposite-of) (value ?fs) (opposite-value ?ns))
  =>
  (duplicate-instance ?node
    (search-depth (+ 1 ?num))
    (parent ?node)
    (farmer-location ?ns)
    (last-move goat)
    (goat-location ?ns)))

(defrule move-with-cabbage
  ?node <- (object (is-a status)
                   (search-depth ?num) 
                   (farmer-location ?fs)
                   (cabbage-location ?fs))
  (object (is-a opposite-of) (value ?fs) (opposite-value ?ns))
  =>
  (duplicate-instance ?node
    (search-depth (+ 1 ?num))
    (parent ?node)
    (farmer-location ?ns)
    (last-move cabbage)
    (cabbage-location ?ns)))

;;;******************************
;;;* CONSTRAINT VIOLATION RULES *
;;;******************************

(defrule fox-eats-goat 
  (declare (salience 200))
  ?node <- (object (is-a status)
                   (farmer-location ?s1)
                   (fox-location ?s2&~?s1)
                   (goat-location ?s2))
  =>
  (unmake-instance ?node))

(defrule goat-eats-cabbage 
  (declare (salience 200))
  ?node <- (object (is-a status)
                   (farmer-location ?s1)
                   (goat-location ?s2&~?s1)
                   (cabbage-location ?s2))
  =>
  (unmake-instance ?node))

(defrule circular-path 
  (declare (salience 200))
  (object (is-a status)
          (search-depth ?sd1)
          (farmer-location ?fs)
          (fox-location ?xs)
          (goat-location ?gs)
          (cabbage-location ?cs))
  ?node <- (object (is-a status)
                   (search-depth ?sd2&:(< ?sd1 ?sd2))
                   (farmer-location ?fs)
                   (fox-location ?xs)
                   (goat-location ?gs)
                   (cabbage-location ?cs))
  =>
  (unmake-instance ?node))

;;;*********************************
;;;* FIND AND PRINT SOLUTION RULES *
;;;*********************************

(defrule recognize-solution 
  (declare (salience 100))
  ?node <- (object (is-a status)
                   (parent ?parent)
                   (farmer-location shore-2)
                   (fox-location shore-2)
                   (goat-location shore-2)
                   (cabbage-location shore-2)
                   (last-move ?move))
  =>
  (unmake-instance ?node)
  (make-instance of moves
     (id ?parent) (moves-list ?move)))

(defrule further-solution 
  (declare (salience 100))
  ?state <- (object (is-a status)
                    (parent ?parent)
                    (last-move ?move))
  ?mv <- (object (is-a moves)
                 (id ?state)
                 (moves-list $?rest))
  =>
  (modify-instance ?mv (id ?parent) (moves-list ?move ?rest)))

(defrule print-solution 
  (declare (salience 100))
  ?mv <- (object (is-a moves)
                 ;(id [no-parent]) 
                 (moves-list no-move $?m))
  =>
  (unmake-instance ?mv)
  ;(printout t crlf "Solution found: " crlf crlf)
  (bind ?length (length ?m))
  (bind ?i 1)
  (bind ?shore shore-2)
  (while (<= ?i ?length)
     (bind ?thing (nth$ ?i ?m))
     (if (eq ?thing alone)
        then (printout t "Farmer moves alone to " ?shore "." crlf)
        else (printout t "Farmer moves with " ?thing " to " ?shore "." crlf))
     (if (eq ?shore shore-1)
        then (bind ?shore shore-2)
        else (bind ?shore shore-1))
     (bind ?i (+ 1 ?i))))
