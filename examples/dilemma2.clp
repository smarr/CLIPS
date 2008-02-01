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
;;;        This example uses COOL classes and 
;;;     message-handlers to solve the problem.
;;;
;;;     CLIPS Version 6.0 Example
;;; 
;;;     To execute, merely load and enter (solve-dilemma).
;;;======================================================

;;;**************
;;;* DEFCLASSES *
;;;**************

(defclass status
   (is-a USER)
   (role concrete)
   (slot farmer
      (create-accessor write)
      (default shore-1))
   (slot fox
      (create-accessor write)
      (default shore-1))
   (slot goat
      (create-accessor write)
      (default shore-1))
   (slot cabbage
      (create-accessor write)
      (default shore-1))
   (slot parent
      (create-accessor write)
      (default no-parent))
   (slot search-depth
      (create-accessor write)
      (default 1))
   (slot last-move
      (create-accessor write)
      (default no-move)))

;;;****************
;;;* DEFFUNCTIONS *
;;;****************

(deffunction contradiction
   (?f ?x ?g ?c ?d)
   (if (or (and (eq ?x ?g) (neq ?f ?x)) (and (eq ?g ?c) (neq ?f ?g)))
      then
      TRUE
      else
      (any-instancep ((?s status))
        (and (eq ?s:farmer ?f) 
          (eq ?s:fox ?x)
          (eq ?s:goat ?g)
          (eq ?s:cabbage ?c)
          (< ?s:search-depth ?d)))))

(deffunction opposite-shore
   (?value)
   (if (eq ?value shore-1)
      then
      shore-2
      else
      shore-1))

(deffunction solve-dilemma ()
   (do-for-all-instances ((?a status))
      TRUE
      (send ?a delete))  
   (make-instance start of status)
   (send [start] generate-moves))

;;;**************
;;;* DEFRULES *
;;;**************

(defrule start-it
  =>
  (solve-dilemma))

;;;***********************
;;;* DEFMESSAGE-HANDLERS *
;;;***********************

(defmessage-handler status move-farmer
   ()
   (if (not (contradiction (opposite-shore ?self:farmer) ?self:fox 
                           ?self:goat ?self:cabbage ?self:search-depth))
      then
      (bind ?x (make-instance (gensym) of status
         (farmer (opposite-shore ?self:farmer))
         (fox ?self:fox)
         (goat ?self:goat)
         (cabbage ?self:cabbage)
         (last-move farmer)
         (parent ?self)
         (search-depth (+ ?self:search-depth 1))))
      (if (not (send ?x solution?))
         then
         (send ?x generate-moves))))

(defmessage-handler status move-goat
   ()
   (if (and (eq ?self:farmer ?self:goat) (not (contradiction 
      (opposite-shore ?self:farmer) ?self:fox (opposite-shore ?self:goat) 
       ?self:cabbage ?self:search-depth)))
      then
      (bind ?x (make-instance (gensym) of status
         (farmer (opposite-shore ?self:farmer))
         (fox ?self:fox)
         (goat (opposite-shore ?self:farmer))
         (cabbage ?self:cabbage)
         (last-move goat)
         (parent ?self)
         (search-depth (+ ?self:search-depth 1))))
      (if (not (send ?x solution?))
         then
         (send ?x generate-moves))))

(defmessage-handler status move-fox
   ()
   (if (and (eq ?self:farmer ?self:fox) 
            (not (contradiction (opposite-shore ?self:farmer) 
                                (opposite-shore ?self:fox) 
                                ?self:goat ?self:cabbage ?self:search-depth)))
      then
      (bind ?x (make-instance (gensym) of status
         (farmer (opposite-shore ?self:farmer))
         (fox (opposite-shore ?self:farmer))
         (goat ?self:goat)
         (cabbage ?self:cabbage)
         (last-move fox)
         (parent ?self)
         (search-depth (+ ?self:search-depth 1))))
      (if (not (send ?x solution?))
         then
         (send ?x generate-moves))))

(defmessage-handler status move-cabbage
   ()
   (if (and (eq ?self:farmer ?self:cabbage) 
            (not (contradiction (opposite-shore ?self:farmer) 
                                ?self:fox ?self:goat 
                                (opposite-shore ?self:cabbage) 
                                ?self:search-depth)))
      then
      (bind ?x (make-instance (gensym) of status
         (farmer (opposite-shore ?self:farmer))
         (fox ?self:fox)
         (goat ?self:goat)
         (cabbage (opposite-shore ?self:farmer))
         (last-move cabbage)
         (parent ?self)
         (search-depth (+ ?self:search-depth 1))))
      (if (not (send ?x solution?))
         then
         (send ?x generate-moves))))

(defmessage-handler status generate-moves
   ()
   (send ?self move-farmer)
   (send ?self move-fox)
   (send ?self move-goat)
   (send ?self move-cabbage))

(defmessage-handler status print-solution
   ()
   (if (neq ?self:parent no-parent)
      then
      (send ?self:parent print-solution)
      (bind ?move-dest (dynamic-get ?self:last-move))
      (if (eq ?self:last-move farmer)
         then
         (printout t "Farmer moves alone to " ?move-dest "." crlf)
         else
         (printout t "Farmer moves with " ?self:last-move " to " ?move-dest "." crlf))))

(defmessage-handler status solution?
   ()
   (if (and (eq ?self:farmer shore-2) (eq ?self:fox shore-2) 
            (eq ?self:goat shore-2) (eq ?self:cabbage shore-2))
      then
      (printout t crlf "Solution found:" crlf crlf)
      (send ?self print-solution)
      TRUE
      else
      FALSE))


