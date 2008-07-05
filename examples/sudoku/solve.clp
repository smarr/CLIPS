;;; Version 1.2
;;; 
;;; JRules Changes

;;; Version 1.1
;;;
;;; Added Unique Rectangles

;;; #######################
;;; DEFTEMPLATES & DEFFACTS
;;; #######################

(deftemplate position-value-color
   (slot row)
   (slot column)
   (slot group)
   (slot id)
   (slot value)
   (slot color))

(deftemplate chain
   (slot start-row)
   (slot start-column)
   (slot start-value)
   (slot row)
   (slot column)
   (slot group)
   (slot id)
   (slot value))

;;; ####################
;;; INITIALIZATION RULES
;;; ####################


;;; *********************
;;; initialize-techniques
;;; *********************

(defrule initialize-techniques
  
  (declare (salience 10))

   =>
   
   (assert (technique (name Naked-Single) (rank 1)))
   (assert (technique (name Hidden-Single) (rank 2)))
   (assert (technique (name Locked-Candidate-Single-Line) (rank 3)))
   (assert (technique (name Locked-Candidate-Multiple-Lines) (rank 4)))
   (assert (technique (name Naked-Pairs) (rank 5)))
   (assert (technique (name Hidden-Pairs) (rank 6)))
   (assert (technique (name X-Wing) (rank 7)))
   (assert (technique (name Naked-Triples) (rank 8)))
   (assert (technique (name Hidden-Triples) (rank 9)))
   (assert (technique (name XY-Wing) (rank 10)))
   (assert (technique (name Swordfish) (rank 11)))
   (assert (technique (name Duplicate-Color) (rank 12)))
   (assert (technique (name Color-Conjugate-Pair) (rank 13)))
   (assert (technique (name Multi-Color-Type-1) (rank 14)))
   (assert (technique (name Multi-Color-Type-2) (rank 15)))
   (assert (technique (name Forced-Chain-Convergence) (rank 16)))
   (assert (technique (name Forced-Chain-XY) (rank 17)))
   (assert (technique (name Unique-Rectangle) (rank 18))))
   
;;; #################
;;; ELIMINATION RULES
;;; #################

;;; *************
;;; remove-colors
;;; *************

(defrule remove-colors ""

   (declare (salience 20))

   (phase elimination)
   
   ?f <- (position-value-color)

   =>
   
   (retract ?f))

;;; *************
;;; remove-chains
;;; *************

(defrule remove-chains ""

   (declare (salience 20))

   (phase elimination)
   
   ?f <- (chain)

   =>
   
   (retract ?f))

;;; ***************
;;; remove-unsolved
;;; ***************
   
(defrule remove-unsolved
   
   (declare (salience 20))

   (phase elimination)

   ?f <- (unsolved (row ?r) (column ?c))
   
   (possible (row ?r) (column ?c) (value ?v))
   
   (not (possible (row ?r) (column ?c) (value ~?v)))
   
   =>
         
   (retract ?f))
 
;;; **********************
;;; eliminate-not-employed
;;; **********************

(defrule eliminate-not-employed

   (declare (salience 10))

   (phase elimination)
   
   ?f1 <- (impossible (id ?id) (value ?v) (rank ?p) (reason ?r))
   
   (not (impossible (id ?id2&:(< ?id2 ?id))))
   
   (not (impossible (id ?id) (value ?v2&:(< ?v2 ?v))))
   
   (not (impossible (id ?id) (value ?v) (rank ?p2&:(< ?p2 ?p))))
   
   ?f2 <- (possible (id ?id) (value ?v))
   
   (not (technique-employed (rank ?p)))

   =>
   
   (retract ?f1 ?f2)
   
   (assert (technique-employed (rank ?p) (reason ?r))))

;;; ******************
;;; eliminate-employed
;;; ******************

(defrule eliminate-employed

   (declare (salience 10))

   (phase elimination)
   
   ?f1 <- (impossible (id ?id) (value ?v) (rank ?p) (reason ?r))
   
   (not (impossible (id ?id2&:(< ?id2 ?id))))
   
   (not (impossible (id ?id) (value ?v2&:(< ?v2 ?v))))
   
   (not (impossible (id ?id) (value ?v) (rank ?p2&:(< ?p2 ?p))))
   
   ?f2 <- (possible (id ?id) (value ?v))
   
   (exists (technique-employed (rank ?p)))

   =>
   
   (retract ?f1 ?f2))
  
;;; ************
;;; remove-extra
;;; ************

(defrule remove-extra

   (declare (salience 10))
   
   (phase elimination)
   
   ?f <- (impossible (id ?id) (value ?v))
   
   (not (possible (id ?id) (value ?v)))
   
   =>
   
   (retract ?f))
   
;;; ****************
;;; elimination-done
;;; ****************

(defrule elimination-done

   (declare (salience 10))
   
   ?f <- (phase elimination)
   
   (not (impossible))
   
   =>
   
   (retract ?f)
   
   (assert (phase match)))

;;; ###############
;;; TECHNIQUE RULES
;;; ###############

;;; #############
;;; Naked Singles
;;; #############

;;; ******************
;;; naked-single-group
;;; ******************

(defrule naked-single-group
   
   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Naked-Single) (rank ?p))
   
   (possible (value ?v) (group ?g) (id ?id))
   
   (not (possible (value ~?v) (group ?g) (id ?id)))
   
   (possible (value ?v) (group ?g) (id ?id2&~?id))
   
   (not (impossible (id ?id2) (value ?v) (rank ?p)))
   
   =>
   
   (assert (impossible (id ?id2) (value ?v) (rank ?p) (reason "Naked Single"))))
   
;;; ****************
;;; naked-single-row
;;; ****************

(defrule naked-single-row
   
   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Naked-Single) (rank ?p))
   
   (possible (value ?v) (row ?r) (id ?id))
   
   (not (possible (value ~?v) (row ?r) (id ?id)))
   
   (possible (value ?v) (row ?r) (id ?id2&~?id))
   
   (not (impossible (id ?id2) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id2) (value ?v) (rank ?p) (reason "Naked Single"))))
   
;;; *******************
;;; naked-single-column
;;; *******************

(defrule naked-single-column
   
   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Naked-Single) (rank ?p))
   
   (possible (value ?v) (column ?c) (id ?id))
   
   (not (possible (value ~?v) (column ?c) (id ?id)))
   
   (possible (value ?v) (column ?c) (id ?id2&~?id))
   
   (not (impossible (id ?id2) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id2) (value ?v) (rank ?p) (reason "Naked Single"))))

;;; ##############
;;; Hidden Singles
;;; ##############
   
;;; *******************
;;; hidden-single-group
;;; *******************

(defrule hidden-single-group
   
   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Hidden-Single) (rank ?p))
   
   (possible (value ?v) (group ?g) (id ?id))
   
   (not (possible (value ?v) (group ?g) (id ~?id)))
   
   (possible (value ?v2&~?v) (group ?g) (id ?id))
   
   (not (impossible (id ?id) (value ?v2) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v2) (rank ?p) (reason "Hidden Single"))))
   
;;; *****************
;;; hidden-single-row
;;; *****************

(defrule hidden-single-row
   
   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Hidden-Single) (rank ?p))
   
   (possible (value ?v) (row ?r) (id ?id))
   
   (not (possible (value ?v) (row ?r) (id ~?id)))
   
   (possible (value ?v2&~?v) (row ?r) (id ?id))
   
   (not (impossible (id ?id) (value ?v2) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v2) (rank ?p) (reason "Hidden Single"))))
   
;;; ********************
;;; hidden-single-column
;;; ********************

(defrule hidden-single-column
   
   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Hidden-Single) (rank ?p))
   
   (possible (value ?v) (column ?c) (id ?id))
   
   (not (possible (value ?v) (column ?c) (id ~?id)))
   
   (possible (value ?v2&~?v) (column ?c) (id ?id))
   
   (not (impossible (id ?id) (value ?v2) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v2) (rank ?p) (reason "Hidden Single"))))

;;; ############################
;;; Locked Candidate Single Line
;;; ############################

;;; ********************************
;;; locked-candidate-single-line-row
;;; ********************************

(defrule locked-candidate-single-line-row

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Locked-Candidate-Single-Line) (rank ?p))
   
   (possible (value ?v) (row ?r) (group ?g))
   
   (not (possible (value ?v) (row ~?r) (group ?g)))
   
   (possible (value ?v) (row ?r) (group ~?g) (id ?id))
   
   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "Locked Candidate Single Line"))))

;;; ***********************************
;;; locked-candidate-single-line-column
;;; ***********************************

(defrule locked-candidate-single-line-column

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Locked-Candidate-Single-Line) (rank ?p))
   
   (possible (value ?v) (column ?c) (group ?g))
   
   (not (possible (value ?v) (column ~?c) (group ?g)))
   
   (possible (value ?v) (column ?c) (group ~?g) (id ?id))
   
   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "Locked Candidate Single Line"))))

;;; ###############################
;;; Locked Candidate Multiple Lines
;;; ###############################

;;; ***********************************
;;; locked-candidate-multiple-lines-row
;;; ***********************************

(defrule locked-candidates-multiple-lines-row

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Locked-Candidate-Multiple-Lines) (rank ?p))
   
   (possible (value ?v) (row ?r) (group ?g))
   
   (not (possible (value ?v) (row ?r) (group ~?g)))
   
   (possible (value ?v) (row ~?r) (group ?g) (id ?id))
   
   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "Locked Candidate Multiple Lines"))))

;;; **************************************
;;; locked-candidate-multiple-lines-column
;;; **************************************

(defrule locked-candidate-multiple-lines-column

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Locked-Candidate-Multiple-Lines) (rank ?p))
   
   (possible (value ?v) (column ?c) (group ?g))
   
   (not (possible (value ?v) (column ?c) (group ~?g)))
   
   (possible (value ?v) (column ~?c) (group ?g) (id ?id))
   
   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "Locked Candidate Multiple Lines"))))


;;; ###########
;;; Naked Pairs
;;; ###########

;;; ***************
;;; naked-pairs-row
;;; ***************

(defrule naked-pairs-row

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Naked-Pairs) (rank ?p))
   
   (possible (value ?v1) (row ?r) (column ?c1))
   
   (possible (value ?v2&~?v1) (row ?r) (column ?c1))
   
   (not (possible (value ~?v2&~?v1) (row ?r) (column ?c1)))
   
   (possible (value ?v1) (row ?r) (column ?c2&~?c1))
   
   (possible (value ?v2) (row ?r) (column ?c2))
   
   (not (possible (value ~?v2&~?v1) (row ?r) (column ?c2)))

   (possible (value ?v& ?v1 | ?v2) (row ?r) (column ~?c1&~?c2) (id ?id))

   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "Naked Pairs"))))

;;; ******************
;;; naked-pairs-column
;;; ******************

(defrule naked-pairs-column

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Naked-Pairs) (rank ?p))

   (possible (value ?v1) (row ?r1) (column ?c))
   
   (possible (value ?v2&~?v1) (row ?r1) (column ?c))
   
   (not (possible (value ~?v2&~?v1) (row ?r1) (column ?c)))
   
   (possible (value ?v1) (row ?r2&~?r1) (column ?c))
   
   (possible (value ?v2) (row ?r2) (column ?c))
   
   (not (possible (value ~?v2&~?v1) (row ?r2) (column ?c)))

   (possible (value ?v& ?v1 | ?v2) (row ~?r1&~?r2) (column ?c) (id ?id))

   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "Naked Pairs"))))

;;; *****************
;;; naked-pairs-group
;;; *****************

(defrule naked-pairs-group

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Naked-Pairs) (rank ?p))

   (possible (value ?v1) (group ?g) (id ?id1))
   
   (possible (value ?v2&~?v1) (id ?id1))
   
   (not (possible (value ~?v2&~?v1) (id ?id1)))
   
   (possible (value ?v1) (group ?g) (id ?id2&~?id1))
   
   (possible (value ?v2) (id ?id2))
   
   (not (possible (value ~?v2&~?v1) (id ?id2)))

   (possible (value ?v& ?v1 | ?v2) (group ?g) (id ?id&~?id2&~?id1))

   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "Naked Pairs"))))

;;; ############
;;; Hidden Pairs
;;; ############

;;; ****************
;;; hidden-pairs-row
;;; ****************

(defrule hidden-pairs-row

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Hidden-Pairs) (rank ?p))
   
   (possible (value ?v1) (row ?r) (column ?c1))
   
   (possible (value ?v2&~?v1) (row ?r) (column ?c1))
      
   (possible (value ?v1) (row ?r) (column ?c2&~?c1))
   
   (possible (value ?v2) (row ?r) (column ?c2))
   
   (not (possible (value ?v1 | ?v2) (row ?r) (column ~?c2&~?c1)))

   (possible (value ?v&~?v1&~?v2) (row ?r) (column ?c1 | ?c2) (id ?id))

   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "Hidden Pairs"))))

;;; *******************
;;; hidden-pairs-column
;;; *******************

(defrule hidden-pairs-column

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Hidden-Pairs) (rank ?p))
   
   (possible (value ?v1) (row ?r1) (column ?c))
   
   (possible (value ?v2&~?v1) (row ?r1) (column ?c))
      
   (possible (value ?v1) (row ?r2&~?r1) (column ?c))
   
   (possible (value ?v2) (row ?r2) (column ?c))
   
   (not (possible (value ?v1 | ?v2) (row ~?r2&~?r1) (column ?c)))

   (possible (value ?v&~?v1&~?v2) (row ?r1 | ?r2) (column ?c) (id ?id))

   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "Hidden Pairs"))))


;;; ******************
;;; hidden-pairs-group
;;; ******************

(defrule hidden-pairs-group

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Hidden-Pairs) (rank ?p))
   
   (possible (value ?v1) (group ?g) (id ?id1))
   
   (possible (value ?v2&~?v1) (id ?id1))
      
   (possible (value ?v1) (group ?g) (id ?id2&~?id1))
   
   (possible (value ?v2) (id ?id2))
   
   (not (possible (value ?v1 | ?v2) (group ?g) (id ~?id2&~?id1)))

   (possible (value ?v&~?v1&~?v2) (id ?id&?id1 | ?id2))

   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "Hidden Pairs"))))


;;; ######
;;; X Wing
;;; ######

;;; **********
;;; X-Wing-Row
;;; **********

(defrule X-Wing-Row

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name X-Wing) (rank ?p))
   
   (possible (value ?v) (row ?r1) (column ?c1))
   
   (possible (value ?v) (row ?r1) (column ?c2&~?c1))
   
   (not (possible (value ?v) (row ?r1) (column ~?c1&~?c2)))
   
   (possible (value ?v) (row ?r2&~?r1) (column ?c1))
   
   (possible (value ?v) (row ?r2) (column ?c2))
   
   (not (possible (value ?v) (row ?r2) (column ~?c1&~?c2)))
   
   (possible (value ?v) (row ~?r1&~?r2) (column ?c1 | ?c2) (id ?id))
   
   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "X Wing"))))

;;; *************
;;; X-Wing-Column
;;; *************

(defrule X-Wing-Column

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name X-Wing) (rank ?p))
   
   (possible (value ?v) (row ?r1) (column ?c1))
   
   (possible (value ?v) (row ?r2&~?r1) (column ?c1))
   
   (not (possible (value ?v) (row ~?r1&~?r2) (column ?c1)))
   
   (possible (value ?v) (row ?r1) (column ?c2&~?c1))
   
   (possible (value ?v) (row ?r2) (column ?c2))
   
   (not (possible (value ?v) (row ~?r1&~?r2) (column ?c2)))
   
   (possible (value ?v) (row ?r1 | ?r2) (column ~?c1&~?c2) (id ?id))
   
   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "X Wing"))))

;;; #############
;;; Naked Triples
;;; #############

;;; ****************
;;; generate-triples
;;; ****************

(defrule generate-triples
   
   (rank (value ?p) (process yes))

   (technique (name Naked-Triples) (rank ?p))

   (size ?s)
   
   (size-value (size ?sv1&:(<= ?sv1 ?s)) (value ?v1))
 
   (size-value (size ?sv2&:(<= ?sv2 ?s)) (value ?v2&:(> ?v2 ?v1)))

   (size-value (size ?sv3&:(<= ?sv3 ?s)) (value ?v3&:(> ?v3 ?v2)))

   =>
   
   (assert (triple ?v1 ?v2 ?v3)))

;;; *****************
;;; naked-triples-row
;;; *****************

(defrule naked-triples-row

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Naked-Triples) (rank ?p))
   
   (triple ?v1 ?v2 ?v3)
   
   (possible (value ?v1) (row ?r) (id ?id1))
   
   (not (possible (value ~?v1&~?v2&~?v3) (id ?id1)))
   
   (possible (value ?v2) (row ?r) (id ?id2&~?id1))

   (not (possible (value ~?v1&~?v2&~?v3) (id ?id2)))
   
   (possible (value ?v3) (row ?r) (id ?id3&~?id2&~?id1))
   
   (not (possible (value ~?v1&~?v2&~?v3) (id ?id3)))
   
   (possible (value ?v& ?v1 | ?v2 | ?v3) (row ?r) (id ?id&~?id1&~?id2&~?id3))

   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "Naked Triples"))))

;;; ********************
;;; naked-triples-column
;;; ********************

(defrule naked-triples-column

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Naked-Triples) (rank ?p))
   
   (triple ?v1 ?v2 ?v3)
   
   (possible (value ?v1) (column ?c) (id ?id1))
   
   (not (possible (value ~?v1&~?v2&~?v3) (id ?id1)))
   
   (possible (value ?v2) (column ?c) (id ?id2&~?id1))

   (not (possible (value ~?v1&~?v2&~?v3) (id ?id2)))
   
   (possible (value ?v3) (column ?c) (id ?id3&~?id2&~?id1))
   
   (not (possible (value ~?v1&~?v2&~?v3) (id ?id3)))
   
   (possible (value ?v& ?v1 | ?v2 | ?v3) (column ?c) (id ?id&~?id1&~?id2&~?id3))

   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "Naked Triples"))))

;;; *******************
;;; naked-triples-group
;;; *******************

(defrule naked-triples-group

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Naked-Triples) (rank ?p))
   
   (triple ?v1 ?v2 ?v3)
   
   (possible (value ?v1) (group ?g) (id ?id1))
   
   (not (possible (value ~?v1&~?v2&~?v3) (id ?id1)))
   
   (possible (value ?v2) (group ?g) (id ?id2&~?id1))

   (not (possible (value ~?v1&~?v2&~?v3) (id ?id2)))
   
   (possible (value ?v3) (group ?g) (id ?id3&~?id2&~?id1))
   
   (not (possible (value ~?v1&~?v2&~?v3) (id ?id3)))
   
   (possible (value ?v& ?v1 | ?v2 | ?v3) (group ?g) (id ?id&~?id1&~?id2&~?id3))

   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "Naked Triples"))))

;;; ##############
;;; Hidden Triples
;;; ##############

;;; ******************
;;; hidden-triples-row
;;; ******************

(defrule hidden-triples-row

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Hidden-Triples) (rank ?p))
   
   (triple ?v1 ?v2 ?v3)
   
   (possible (value ?v1) (row ?r) (column ?c1))
   
   (possible (value ?v2) (row ?r) (column ?c2&~?c1))
   
   (possible (value ?v3) (row ?r) (column ?c3&~?c2&~?c1))
   
   (not (possible (value ?v1 | ?v2 | ?v3) (row ?r) (column ~?c3&~?c2&~?c1)))
   
   (possible (value ?v&~?v1&~?v2&~?v3) (row ?r) (column ?c1 | ?c2 | ?c3) (id ?id))

   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "Hidden Triples"))))

;;; *********************
;;; hidden-triples-column
;;; *********************

(defrule hidden-triples-column

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Hidden-Triples) (rank ?p))
   
   (triple ?v1 ?v2 ?v3)
   
   (possible (value ?v1) (row ?r1) (column ?c))
   
   (possible (value ?v2) (row ?r2&~?r1) (column ?c))
   
   (possible (value ?v3) (row ?r3&~?r2&~?r1) (column ?c))
   
   (not (possible (value ?v1 | ?v2 | ?v3) (row ~?r3&~?r2&~?r1) (column ?c)))
   
   (possible (value ?v&~?v1&~?v2&~?v3) (row ?r1 | ?r2 | ?r3) (column ?c) (id ?id))

   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "Hidden Triples"))))

;;; ********************
;;; hidden-triples-group
;;; ********************

(defrule hidden-triples-group

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Hidden-Triples) (rank ?p))
   
   (triple ?v1 ?v2 ?v3)
   
   (possible (value ?v1) (id ?id1) (group ?g))
   
   (possible (value ?v2) (id ?id2&~?id1) (group ?g))
   
   (possible (value ?v3) (id ?id3&~?id2&~?id1) (group ?g))
   
   (not (possible (value ?v1 | ?v2 | ?v3) (id ~?id3&~?id2&~?id1) (group ?g)))
   
   (possible (value ?v&~?v1&~?v2&~?v3) (id ?id& ?id1 | ?id2 | ?id3))

   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "Hidden Triples"))))

;;; #########
;;; Swordfish
;;; #########

;;; *************
;;; swordfish-row
;;; *************

(defrule swordfish-row

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Swordfish) (rank ?p))
   
   (triple ?c1 ?c2 ?c3)
   
   (possible (value ?v) (row ?r1) (column ?c1))
   
   (not (possible (value ?v) (row ?r1) (column ~?c1&~?c2&~?c3)))
   
   (possible (value ?v) (row ?r2&~?r1) (column ?c2))

   (not (possible (value ?v) (row ?r2) (column ~?c1&~?c2&~?c3)))
   
   (possible (value ?v) (row ?r3&~?r2&~?r1) (column ?c3))

   (not (possible (value ?v) (row ?r3) (column ~?c1&~?c2&~?c3)))
   
   (possible (value ?v) (row ~?r1&~?r2&~?r3) (column ?c1 | ?c2 | ?c3) (id ?id))

   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "Swordfish"))))

;;; ****************
;;; swordfish-column
;;; ****************

(defrule swordfish-column

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Swordfish) (rank ?p))
   
   (triple ?r1 ?r2 ?r3)
   
   (possible (value ?v) (row ?r1) (column ?c1))
   
   (not (possible (value ?v) (row ~?r1&~?r2&~?r3) (column ?c1)))
   
   (possible (value ?v) (row ?r2) (column ?c2&~?c1))

   (not (possible (value ?v) (row ~?r1&~?r2&~?r3) (column ?c2)))
   
   (possible (value ?v) (row ?r3) (column ?c3&~?c2&~?c1))

   (not (possible (value ?v) (row ~?r1&~?r2&~?r3) (column ?c3)))
   
   (possible (value ?v) (row ?r1 | ?r2 | ?r3) (column ~?c1&~?c2&~?c3) (id ?id))

   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "Swordfish"))))

;;; #######
;;; XY-Wing
;;; #######

;;; *******
;;; XY-Wing
;;; *******
 
(defrule XY-Wing

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name XY-Wing) (rank ?p))
   
   (possible (value ?x) (row ?r1) (column ?c1) (group ?g1) (id ?id1))
   
   (possible (value ?y&~?x) (id ?id1))
   
   (not (possible (value ~?y&~?x) (id ?id1)))
   
   (possible (value ?x) (row ?r2) (column ?c2) (group ?g2) (id ?id2&~?id1))
   
   (possible (value ?z&~?x) (id ?id2))
   
   (not (possible (value ~?z&~?x) (id ?id2)))
               
   (test (or (= ?r1 ?r2) (= ?c1 ?c2) (= ?g1 ?g2)))

   (possible (value ?y) (row ?r3) (column ?c3) (group ?g3) (id ?id3&~?id2&~?id1))
   
   (possible (value ?z&~?y) (id ?id3))
   
   (not (possible (value ~?z&~?y) (id ?id3)))
             
   (test (or (= ?r1 ?r3) (= ?c1 ?c3) (= ?g1 ?g3)))
                            
   (possible (value ?z) (row ?r4) (column ?c4) (group ?g4) (id ?id&~?id3&~?id2&~?id1))

   (test (and (or (= ?r2 ?r4) (= ?c2 ?c4) (= ?g2 ?g4))
              (or (= ?r3 ?r4) (= ?c3 ?c4) (= ?g3 ?g4))))

   (not (impossible (id ?id) (value ?z) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?z) (rank ?p) (reason "XY-Wing"))))

;;; ######
;;; Colors
;;; ######

;;; **********************
;;; initialize-color-pairs
;;; **********************

(defrule initialize-color-pairs
  
  (phase match)
   
  (rank (value ?p) (process yes))

  (technique (name Duplicate-Color | Color-Conjugate-Pair | Multi-Color-Type-1  | Multi-Color-Type-2) 
             (rank ?p))
  
  (not (color-pair ? ?))

  =>
  
  (assert (color-pair green magenta))
  (assert (color-pair magenta green))
  (assert (color-pair orange azure))
  (assert (color-pair azure orange))
  (assert (color-pair violet chartruese))
  (assert (color-pair chartruese violet))
  (assert (color-pair aquamarine fuchsia))   
  (assert (color-pair fuchsia aquamarine))  
  (assert (color-pair yellow blue))
  (assert (color-pair blue yellow))
  (assert (color-pair red cyan))
  (assert (color-pair cyan red)))

;;; *********
;;; color-row
;;; *********

(defrule color-row

   (declare (salience -10))
   
   (phase match)
   
   (rank (value ?p) (process yes))

   (technique (name Duplicate-Color | Color-Conjugate-Pair | Multi-Color-Type-1  | Multi-Color-Type-2) 
              (rank ?p))

   (possible (row ?r) (column ?c1) (group ?g1) (id ?id1) (value ?v))

   (possible (row ?r) (column ?c2&~?c1) (group ?g2) (id ?id2) (value ?v))
   
   (not (possible (row ?r) (column ?c3&~?c2&~?c1) (value ?v)))
                     
   (color-pair ?color1 ?color2)
   
   ;; Don't use a color previously used for this value.

   (not (position-value-color (value ?v)
                              (color ?color1 | ?color2)))
   
   ;; Don't try to color the position if it's already colored. 
   
   (not (position-value-color (row ?r)
                              (column ?c1 | ?c2)
                              (value ?v)))

   =>
  
   (assert (position-value-color (row ?r)
                                 (column ?c1)
                                 (group ?g1)
                                 (id ?id1)
                                 (value ?v)
                                 (color ?color1)))
                  
   (assert (position-value-color (row ?r)
                                 (column ?c2)
                                 (group ?g2)
                                 (id ?id2)
                                 (value ?v)
                                 (color ?color2))))

;;; ************
;;; color-column
;;; ************

(defrule color-column

   (declare (salience -10))
   
   (phase match)
   
   (rank (value ?p) (process yes))

   (technique (name Duplicate-Color | Color-Conjugate-Pair | Multi-Color-Type-1 | Multi-Color-Type-2) 
              (rank ?p))

   (possible (row ?r1) (column ?c) (group ?g1) (id ?id1) (value ?v))

   (possible (row ?r2&~?r1) (column ?c) (group ?g2) (id ?id2) (value ?v))
   
   (not (possible (row ?r3&~?r2&~?r1) (column ?c) (value ?v)))
                     
   (color-pair ?color1 ?color2)
   
   ;; Don't use a color previously used for this value.

   (not (position-value-color (value ?v)
                              (color ?color1 | ?color2)))
   
   ;; Don't try to color the position if it's already colored. 
   
   (not (position-value-color (row ?r1 | ?r2)
                              (column ?c)
                              (value ?v)))

   =>
   
   (assert (position-value-color (row ?r1)
                                 (column ?c)
                                 (group ?g1)
                                 (id ?id1)
                                 (value ?v)
                                 (color ?color1)))
                  
   (assert (position-value-color (row ?r2)
                                 (column ?c)
                                 (group ?g2)
                                 (id ?id2)
                                 (value ?v)
                                 (color ?color2))))

;;; *******************
;;; propagate-color-row
;;; *******************

(defrule propagate-color-row

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Duplicate-Color | Color-Conjugate-Pair | Multi-Color-Type-1 | Multi-Color-Type-2) 
              (rank ?p))

   (position-value-color (row ?r) (column ?c1) (value ?v) (color ?color1))
                                    
   (possible (row ?r) (column ?c2&~?c1) (group ?g) (id ?id) (value ?v))
      
   (not (position-value-color (row ?r) (column ?c2) (value ?v)))
                              
   (not (possible (row ?r) (column ?c3&~?c2&~?c1) (value ?v)))
                               
   (color-pair ?color1 ?color2)
   
   =>
   
   (assert (position-value-color (row ?r)
                                 (column ?c2)
                                 (group ?g)
                                 (id ?id)
                                 (value ?v)
                                 (color ?color2))))

;;; **********************
;;; propagate-color-column
;;; **********************

(defrule propagate-color-column

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Duplicate-Color | Color-Conjugate-Pair | Multi-Color-Type-1 | Multi-Color-Type-2) 
              (rank ?p))

   (position-value-color (row ?r1) (column ?c) (value ?v) (color ?color1))
                                    
   (possible (row ?r2&~?r1) (column ?c) (group ?g) (id ?id) (value ?v))
      
   (not (position-value-color (row ?r2) (column ?c) (value ?v)))
                              
   (not (possible (row ?r3&~?r2&~?r1) (column ?c) (value ?v)))
                               
   (color-pair ?color1 ?color2)
   
   =>

   (assert (position-value-color (row ?r2)
                                 (column ?c)
                                 (group ?g)
                                 (id ?id)
                                 (value ?v)
                                 (color ?color2))))

;;; *********************
;;; propagate-color-group
;;; *********************

(defrule propagate-color-group

   (phase match)
   
   (rank (value ?p) (process yes))

   (technique (name Duplicate-Color | Color-Conjugate-Pair | Multi-Color-Type-1 | Multi-Color-Type-2) 
              (rank ?p))

   (position-value-color (column ?c1) (row ?r1) (group ?g) (id ?id1) (value ?v) (color ?color1))
                                    
   (possible (column ?c2) (row ?r2) (group ?g) (id ?id2&~?id1) (value ?v))
      
   (not (position-value-color (column ?c2) (row ?r2) (value ?v)))

   (not (possible (group ?g) (id ?id3&~?id2&~?id1) (value ?v)))
   
   (color-pair ?color1 ?color2)
   
   =>

   (assert (position-value-color (column ?c2)
                                 (row ?r2)
                                 (group ?g)
                                 (id ?id2)
                                 (value ?v)
                                 (color ?color2))))

;;; ###############
;;; Duplicate-Color
;;; ###############

;;; **********************
;;; duplicate-color-in-row
;;; **********************

(defrule duplicate-color-in-row

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Duplicate-Color) (rank ?p))

   (position-value-color (row ?r)
                         (column ?c1)
                         (id ?id1)
                         (value ?v)
                         (color ?color))
                                    
   (position-value-color (row ?r)
                         (column ?c2&~?c1)
                         (id ?id2)
                         (value ?v)
                         (color ?color))

   (not (impossible (id ?id1) (value ?v) (rank ?p)))

   =>

   (assert (impossible (id ?id1) (value ?v) (rank ?p) (reason "Duplicate Color"))))
   
   
;;; *************************
;;; duplicate-color-in-column
;;; *************************

(defrule duplicate-color-in-column

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Duplicate-Color) (rank ?p))

   (position-value-color (row ?r1)
                         (column ?c)
                         (id ?id1)
                         (value ?v)
                         (color ?color))
                                    
   (position-value-color (row ?r2&~?r1)
                         (column ?c)
                         (id ?id2)
                         (value ?v)
                         (color ?color))

   (not (impossible (id ?id1) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id1) (value ?v) (rank ?p) (reason "Duplicate Color"))))
   
;;; ************************
;;; duplicate-color-in-group
;;; ************************

(defrule duplicate-color-in-group

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Duplicate-Color) (rank ?p))

   (position-value-color (group ?g)
                         (id ?id1)
                         (value ?v)
                         (color ?color))
                                    
   (position-value-color (group ?g)
                         (id ?id2&~?id1)
                         (value ?v)
                         (color ?color))

   (not (impossible (id ?id1) (value ?v) (rank ?p)))

   =>

   (assert (impossible (id ?id1) (value ?v) (rank ?p) (reason "Duplicate Color"))))

;;; ####################
;;; Color-Conjugate-Pair
;;; ####################

;;; ********************
;;; color-conjugate-pair
;;; ********************

(defrule color-conjugate-pair

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Color-Conjugate-Pair) (rank ?p))

   (color-pair ?color1 ?color2) 

   (position-value-color (row ?r)
                         (column ?pc)
                         (value ?v)
                         (id ?id1)
                         (color ?color1))
                                    
   (position-value-color (column ?c)
                         (row ?pr)
                         (value ?v)
                         (id ?id2&~?id1)
                         (color ?color2))
                         
   (possible (row ?r) (column ?c) (id ?id&~?id2&~?id1) (value ?v))

   (not (impossible (id ?id) (value ?v) (rank ?p)))
                        
   =>
      
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "Color Conjugate Pairs"))))
   
;;; ##################
;;; Multi-Color-Type-1
;;; ##################
   
(defrule multi-color-type-1

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Multi-Color-Type-1) (rank ?p))

   (color-pair ?color1 ?color2) 
   
   (position-value-color (row ?r1)
                         (column ?c1)
                         (group ?g1)
                         (id ?id1)
                         (value ?v)
                         (color ?color1))
                                    
   (position-value-color (row ?r2)
                         (column ?c2)
                         (group ?g2)
                         (id ~?id1)
                         (value ?v)
                         (color ?color2))
                         
   (color-pair ?other-color&~?color1 ~?color1) 
       
   (position-value-color (row ?r3)
                         (column ?c3)
                         (group ?g3)
                         (value ?v)
                         (color ?other-color))
   
   (test (or (= ?r1 ?r3) (= ?c1 ?c3) (= ?g1 ?g3)))
   
   (position-value-color (row ?r4)
                         (column ?c4)
                         (group ?g4)
                         (value ?v)
                         (color ?other-color))
                         
   (test (or (= ?r2 ?r4) (= ?c2 ?c4) (= ?g2 ?g4)))
   
   (position-value-color (id ?id)
                         (value ?v)
                         (color ?other-color))
   
   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>

   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "Multi Color Type 1"))))
 
;;; ##################
;;; Multi-Color-Type-2
;;; ##################
   
(defrule multi-color-type-2

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Multi-Color-Type-2) (rank ?p))

   (color-pair ?color1 ?color2) 
   
   (position-value-color (row ?r1)
                         (column ?c1)
                         (group ?g1)
                         (value ?v)
                         (color ?color1))

   (color-pair ?other-color1&~?color1 ?other-color2&~?color1) 
       
   (position-value-color (row ?r2)
                         (column ?c2)
                         (group ?g2)
                         (value ?v)
                         (color ?other-color1))
   
   (test (or (= ?r1 ?r2) (= ?c1 ?c2) (= ?g1 ?g2)))
   
   (position-value-color (row ?r3)
                         (column ?c3)
                         (group ?g3)
                         (id ?id3)
                         (value ?v)
                         (color ?color2))

   (position-value-color (row ?r4)
                         (column ?c4)
                         (group ?g4)
                         (id ?id4)
                         (value ?v)
                         (color ?other-color2))

   (possible (row ?r5) (column ?c5) (id ?id&~?id3&~?id4) (group ?g5) (value ?v))

   (test (and (or (= ?r3 ?r5) (= ?c3 ?c5) (= ?g3 ?g5))
              (or (= ?r4 ?r5) (= ?c4 ?c5) (= ?g4 ?g5))))
   
   (not (impossible (id ?id) (value ?v) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v) (rank ?p) (reason "Multi Color Type 2"))))

;;; #############
;;; Forced Chains
;;; #############

;;; ***********
;;; start-chain
;;; ***********

(defrule start-chain

   (declare (salience -10))
   
   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Forced-Chain-Convergence | Forced-Chain-XY) (rank ?p))

   (possible (row ?r) (column ?c) (group ?g) (id ?id) (value ?v1))
   
   (possible (id ?id) (value ?v2&~?v1))
   
   (not (possible (id ?id) (value ~?v1&~?v2)))
   
   (not (chain (start-row ?r) (start-column ?c) (start-value ?v1) 
               (row ?r) (column ?c) (value ?v1)))
   
   =>
   
   (assert (chain (start-row ?r)
                  (start-column ?c)
                  (start-value ?v1)
                  (row ?r)
                  (column ?c)
                  (group ?g)
                  (id ?id)
                  (value ?v1))))

;;; ******************
;;; continue-chain-row
;;; ******************

(defrule continue-chain-row

   (declare (salience -10))
   
   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Forced-Chain-Convergence | Forced-Chain-XY) (rank ?p))

   (chain (row ?r) (column ?c1) (value ?v1) (start-row ?sr) (start-column ?sc) (start-value ?sv))
          
   (possible (row ?r) (column ?c2&~?c1) (value ?v1))
   
   (possible (row ?r) (column ?c2) (group ?g) (id ?id) (value ?v2&~?v1))
   
   (not (possible (row ?r) (column ?c2) (value ~?v2&~?v1)))
   
   (not (chain (row ?r) (column ?c2) (value ?v2) 
               (start-row ?sr) (start-column ?sc) (start-value ?sv)))
                 
   =>
   
   (assert (chain (start-row ?sr)
                  (start-column ?sc)
                  (start-value ?sv)
                  (column ?c2)
                  (row ?r)
                  (group ?g)
                  (id ?id)
                  (value ?v2))))

;;; *********************
;;; continue-chain-column
;;; *********************

(defrule continue-chain-column

   (declare (salience -10))
   
   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Forced-Chain-Convergence | Forced-Chain-XY) (rank ?p))
   
   (chain (row ?r1) (column ?c) (value ?v1) (start-row ?sr) (start-column ?sc) (start-value ?sv))
          
   (possible (row ?r2&~?r1) (column ?c) (value ?v1))
   
   (possible (row ?r2) (column ?c) (group ?g) (id ?id) (value ?v2&~?v1))
   
   (not (possible (row ?r2) (column ?c) (value ~?v2&~?v1)))
                 
   (not (chain (row ?r2) (column ?c) (value ?v2) 
               (start-row ?sr) (start-column ?sc) (start-value ?sv)))

   =>

   (assert (chain (start-row ?sr)
                  (start-column ?sc)
                  (start-value ?sv)
                  (row ?r2)
                  (column ?c)
                  (group ?g)
                  (id ?id)
                  (value ?v2))))
   
;;; ********************
;;; continue-chain-group
;;; ********************

(defrule continue-chain-group

   (declare (salience -10))
   
   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Forced-Chain-Convergence | Forced-Chain-XY) (rank ?p))

   (chain (group ?g) (id ?id1) (value ?v1) (start-row ?sr) (start-column ?sc) (start-value ?sv))
          
   (possible (row ?r) (column ?c) (group ?g) (id ?id2&~?id1) (value ?v1))
   
   (possible (id ?id2) (value ?v2&~?v1))
   
   (not (possible (id ?id2) (value ~?v2&~?v1)))
                 
   (not (chain (row ?r) (column ?c) (value ?v2) 
               (start-row ?sr) (start-column ?sc) (start-value ?sv)))

   =>

   (assert (chain (start-row ?sr)
                  (start-column ?sc)
                  (start-value ?sv)
                  (row ?r)
                  (column ?c)
                  (group ?g)
                  (id ?id2)
                  (value ?v2))))

;;; ************************
;;; forced-chain-convergence
;;; ************************

(defrule forced-chain-convergence

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Forced-Chain-Convergence) (rank ?p))

   (chain (start-row ?r1)
          (start-column ?c1)
          (start-value ?v1)
          (row ?r2)
          (column ?c2)
          (value ?v2))
          
   (chain (start-row ?r1)
          (start-column ?c1)
          (start-value ~?v1)
          (row ?r2)
          (column ?c2)
          (value ?v2))
                 
   (possible (row ?r2) (column ?c2) (id ?id) (value ?v3&~?v2))

   (not (impossible (id ?id) (value ?v3) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v3) (rank ?p) (reason "Forced Chain Convergence"))))

;;; ***************
;;; forced-chain-XY
;;; ***************

(defrule forced-chain-XY

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Forced-Chain-XY) (rank ?p))

   (chain (start-row ?r1) (start-column ?c1) (start-value ?v1)
          (row ?r1) (column ?c1) (value ?v1) (id ?id1) (group ?g1))
          
   (chain (start-row ?r1) (start-column ?c1) (start-value ?v2&~?v1)
          (row ?r1) (column ?c1) (value ?v2))
          
   (chain (start-row ?r1) (start-column ?c1) (start-value ?v2)
          (row ?r2) (column ?c2) (group ?g2) (id ?id2&~?id1) (value ?v1))
       
   (possible (row ?r3)
             (column ?c3)
             (id ?id&~?id2&~?id1)
             (group ?g3)
             (value ?v1))
                   
   (test (and (or (= ?g1 ?g3) (= ?r1 ?r3) (= ?c1 ?c3))
              (or (= ?g2 ?g3) (= ?r2 ?r3) (= ?c2 ?c3))))

   (not (impossible (id ?id) (value ?v1) (rank ?p)))

   =>
   
   (assert (impossible (id ?id) (value ?v1) (rank ?p) (reason "Forced Chain XY"))))
   
;;; ################
;;; Unique-Rectangle
;;; ################

;;; ********************
;;; Unique-Rectangle-Row
;;; ********************

(defrule Unique-Rectangle-Row

   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Unique-Rectangle) (rank ?p))
   
   (possible (value ?v1) (group ?g1) (row ?r1) (column ?c1))

   (possible (value ?v2&~?v1) (group ?g1) (row ?r1) (column ?c1))

   (not (possible (value ~?v2&~?v1) (row ?r1) (column ?c1)))
   
   (possible (value ?v1) (group ?g1) (row ?r1) (column ?c2&~?c1))

   (possible (value ?v2) (group ?g1) (row ?r1) (column ?c2))

   (not (possible (value ~?v2&~?v1) (row ?r1) (column ?c2)))
   
   (possible (value ?v1) (group ?g2&~?g1) (row ?r2) (column ?c1))

   (possible (value ?v2) (group ?g2) (row ?r2) (column ?c1))
   
   (not (possible (value ~?v2&~?v1) (group ?g2) (row ?r2) (column ?c1)))  
   
   (possible (value ?v1) (id ?id1) (group ?g2) (row ?r2) (column ?c2))

   (possible (value ?v2) (id ?id2) (group ?g2) (row ?r2) (column ?c2))
   
   (possible (value ~?v2&~?v1) (group ?g2) (row ?r2) (column ?c2)) 
   
   (not (impossible (id ?id1) (value ?v1) (rank ?p)))
   
   =>
   
   (assert (impossible (id ?id1) (value ?v1) (rank ?p) (reason "Unique Rectangle"))))
   
;;; ***********************
;;; Unique-Rectangle-Column
;;; ***********************

(defrule Unique-Rectangle-Column
   
   (phase match)

   (rank (value ?p) (process yes))

   (technique (name Unique-Rectangle) (rank ?p))
   
   (possible (value ?v1) (group ?g1) (row ?r1) (column ?c1))

   (possible (value ?v2&~?v1) (group ?g1) (row ?r1) (column ?c1))

   (not (possible (value ~?v2&~?v1) (row ?r1) (column ?c1)))
   
   (possible (value ?v1) (group ?g1) (row ?r2&~?r1) (column ?c1))

   (possible (value ?v2) (group ?g1) (row ?r2) (column ?c1))

   (not (possible (value ~?v2&~?v1) (row ?r2) (column ?c1)))
   
   (possible (value ?v1) (group ?g2&~?g1) (row ?r1) (column ?c2))

   (possible (value ?v2) (group ?g2) (row ?r1) (column ?c2))
   
   (not (possible (value ~?v2&~?v1) (group ?g2) (row ?r1) (column ?c2)))  
   
   (possible (value ?v1) (id ?id1) (group ?g2) (row ?r2) (column ?c2))

   (possible (value ?v2) (id ?id2) (group ?g2) (row ?r2) (column ?c2))
   
   (possible (value ~?v2&~?v1) (group ?g2) (row ?r2) (column ?c2)) 
   
   (not (impossible (id ?id1) (value ?v1) (rank ?p)))

   =>
   
   (assert (impossible (id ?id1) (value ?v1) (rank ?p) (reason "Unique Rectangle"))))

