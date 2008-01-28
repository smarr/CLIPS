;;; Version 1.1
;;;
;;; Added Unique Rectangles

;;; #######################
;;; DEFTEMPLATES & DEFFACTS
;;; #######################

(deffacts techniques
   (technique (name Naked-Single) (priority 1))
   (technique (name Hidden-Single) (priority 2))
   (technique (name Locked-Candidate-Single-Line) (priority 3))
   (technique (name Locked-Candidate-Multiple-Lines) (priority 4))
   (technique (name Naked-Pairs) (priority 5))
   (technique (name Hidden-Pairs) (priority 6))
   (technique (name X-Wing) (priority 7))
   (technique (name Naked-Triples) (priority 8))
   (technique (name Hidden-Triples) (priority 9))
   (technique (name XY-Wing) (priority 10))
   (technique (name Swordfish) (priority 11))
   (technique (name Duplicate-Color) (priority 12))
   (technique (name Color-Conjugate-Pair) (priority 13))
   (technique (name Multi-Color-Type-1) (priority 14))
   (technique (name Multi-Color-Type-2) (priority 15))
   (technique (name Forced-Chain-Convergence) (priority 16))
   (technique (name Forced-Chain-XY) (priority 17))
   (technique (name Unique-Rectangle) (priority 18)))

(deffacts color-pairs
   (color-pair green magenta)
   (color-pair magenta green)
   (color-pair orange azure)
   (color-pair azure orange)
   (color-pair violet chartruese)
   (color-pair chartruese violet)
   (color-pair aquamarine fuchsia)   
   (color-pair fuchsia aquamarine)   
   (color-pair yellow blue)
   (color-pair blue yellow)
   (color-pair red cyan)
   (color-pair cyan red))

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
   
;;; #################
;;; ELIMINATION RULES
;;; #################

;;; *************
;;; remove-colors
;;; *************

(defrule remove-colors

   (declare (salience 20))

   (phase elimination)
   
   ?f <- (position-value-color)

   =>
   
   (retract ?f))

;;; *************
;;; remove-chains
;;; *************

(defrule remove-chains

   (declare (salience 20))

   (phase elimination)
   
   ?f <- (chain)

   =>
   
   (retract ?f))
 
;;; *********
;;; eliminate
;;; *********

(defrule eliminate

   (declare (salience 10))

   (phase elimination)
   
   ?f1 <- (impossible (id ?id) (value ?v) (priority ?p) (reason ?r))
   
   (not (impossible (id ?id2&:(< ?id2 ?id))))
   
   (not (impossible (id ?id) (value ?v2&:(< ?v2 ?v))))
   
   (not (impossible (id ?id) (value ?v) (priority ?p2&:(< ?p2 ?p))))
   
   ?f2 <- (possible (id ?id) (value ?v))

   =>
   
   (retract ?f1 ?f2)
   
   (assert (technique-employed (priority ?p) (reason ?r))))
      
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
   
   (priority ?p)

   (technique (name Naked-Single) (priority ?p))
   
   (possible (value ?v) (group ?g) (id ?id))
   
   (not (possible (value ~?v) (group ?g) (id ?id)))
   
   (possible (value ?v) (group ?g) (id ?id2&~?id))
   
   =>
   
   (assert (impossible (id ?id2) (value ?v) (priority ?p) (reason "Naked Single"))))
   
;;; ****************
;;; naked-single-row
;;; ****************

(defrule naked-single-row
   
   (priority ?p)

   (technique (name Naked-Single) (priority ?p))
   
   (possible (value ?v) (row ?r) (id ?id))
   
   (not (possible (value ~?v) (row ?r) (id ?id)))
   
   (possible (value ?v) (row ?r) (id ?id2&~?id))
   
   =>
   
   (assert (impossible (id ?id2) (value ?v) (priority ?p) (reason "Naked Single"))))
   
;;; *******************
;;; naked-single-column
;;; *******************

(defrule naked-single-column
   
   (priority ?p)

   (technique (name Naked-Single) (priority ?p))
   
   (possible (value ?v) (column ?c) (id ?id))
   
   (not (possible (value ~?v) (column ?c) (id ?id)))
   
   (possible (value ?v) (column ?c) (id ?id2&~?id))
   
   =>
   
   (assert (impossible (id ?id2) (value ?v) (priority ?p) (reason "Naked Single"))))

;;; ##############
;;; Hidden Singles
;;; ##############
   
;;; *******************
;;; hidden-single-group
;;; *******************

(defrule hidden-single-group
   
   (priority ?p)

   (technique (name Hidden-Single) (priority ?p))
   
   (possible (value ?v) (group ?g) (id ?id))
   
   (not (possible (value ?v) (group ?g) (id ~?id)))
   
   (possible (value ?v2&~?v) (group ?g) (id ?id))
   
   =>
   
   (assert (impossible (id ?id) (value ?v2) (priority ?p) (reason "Hidden Single"))))
   
;;; *****************
;;; hidden-single-row
;;; *****************

(defrule hidden-single-row
   
   (priority ?p)

   (technique (name Hidden-Single) (priority ?p))
   
   (possible (value ?v) (row ?r) (id ?id))
   
   (not (possible (value ?v) (row ?r) (id ~?id)))
   
   (possible (value ?v2&~?v) (row ?r) (id ?id))
   
   =>
   
   (assert (impossible (id ?id) (value ?v2) (priority ?p) (reason "Hidden Single"))))
   
;;; ********************
;;; hidden-single-column
;;; ********************

(defrule hidden-single-column
   
   (priority ?p)

   (technique (name Hidden-Single) (priority ?p))
   
   (possible (value ?v) (column ?c) (id ?id))
   
   (not (possible (value ?v) (column ?c) (id ~?id)))
   
   (possible (value ?v2&~?v) (column ?c) (id ?id))
   
   =>
   
   (assert (impossible (id ?id) (value ?v2) (priority ?p) (reason "Hidden Single"))))

;;; ############################
;;; Locked Candidate Single Line
;;; ############################

;;; ********************************
;;; locked-candidate-single-line-row
;;; ********************************

(defrule locked-candidate-single-line-row
   
   (priority ?p)

   (technique (name Locked-Candidate-Single-Line) (priority ?p))
   
   (possible (value ?v) (row ?r) (group ?g))
   
   (not (possible (value ?v) (row ~?r) (group ?g)))
   
   (possible (value ?v) (row ?r) (group ~?g) (id ?id))
   
   =>
   
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "Locked Candidate Single Line"))))

;;; ***********************************
;;; locked-candidate-single-line-column
;;; ***********************************

(defrule locked-candidate-single-line-column
   
   (priority ?p)

   (technique (name Locked-Candidate-Single-Line) (priority ?p))
   
   (possible (value ?v) (column ?c) (group ?g))
   
   (not (possible (value ?v) (column ~?c) (group ?g)))
   
   (possible (value ?v) (column ?c) (group ~?g) (id ?id))
   
   =>
   
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "Locked Candidate Single Line"))))

;;; ###############################
;;; Locked Candidate Multiple Lines
;;; ###############################

;;; ***********************************
;;; locked-candidate-multiple-lines-row
;;; ***********************************

(defrule locked-candidates-multiple-lines-row

   (priority ?p)

   (technique (name Locked-Candidate-Multiple-Lines) (priority ?p))
   
   (possible (value ?v) (row ?r) (group ?g))
   
   (not (possible (value ?v) (row ?r) (group ~?g)))
   
   (possible (value ?v) (row ~?r) (group ?g) (id ?id))
   
   =>
   
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "Locked Candidate Multiple Lines"))))

;;; **************************************
;;; locked-candidate-multiple-lines-column
;;; **************************************

(defrule locked-candidate-multiple-lines-column

   (priority ?p)

   (technique (name Locked-Candidate-Multiple-Lines) (priority ?p))
   
   (possible (value ?v) (column ?c) (group ?g))
   
   (not (possible (value ?v) (column ?c) (group ~?g)))
   
   (possible (value ?v) (column ~?c) (group ?g) (id ?id))
   
   =>
   
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "Locked Candidate Multiple Lines"))))


;;; ###########
;;; Naked Pairs
;;; ###########

;;; ***************
;;; naked-pairs-row
;;; ***************

(defrule naked-pairs-row

   (priority ?p)

   (technique (name Naked-Pairs) (priority ?p))
   
   (possible (value ?v1) (row ?r) (column ?c1))
   
   (possible (value ?v2&~?v1) (row ?r) (column ?c1))
   
   (not (possible (value ~?v2&~?v1) (row ?r) (column ?c1)))
   
   (possible (value ?v1) (row ?r) (column ?c2&~?c1))
   
   (possible (value ?v2) (row ?r) (column ?c2))
   
   (not (possible (value ~?v2&~?v1) (row ?r) (column ?c2)))

   (possible (value ?v& ?v1 | ?v2) (row ?r) (column ~?c1&~?c2) (id ?id))

   =>
   
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "Naked Pairs"))))

;;; ******************
;;; naked-pairs-column
;;; ******************

(defrule naked-pairs-column

   (priority ?p)

   (technique (name Naked-Pairs) (priority ?p))

   (possible (value ?v1) (row ?r1) (column ?c))
   
   (possible (value ?v2&~?v1) (row ?r1) (column ?c))
   
   (not (possible (value ~?v2&~?v1) (row ?r1) (column ?c)))
   
   (possible (value ?v1) (row ?r2&~?r1) (column ?c))
   
   (possible (value ?v2) (row ?r2) (column ?c))
   
   (not (possible (value ~?v2&~?v1) (row ?r2) (column ?c)))

   (possible (value ?v& ?v1 | ?v2) (row ~?r1&~?r2) (column ?c) (id ?id))

   =>
   
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "Naked Pairs"))))

;;; *****************
;;; naked-pairs-group
;;; *****************

(defrule naked-pairs-group

   (priority ?p)

   (technique (name Naked-Pairs) (priority ?p))

   (possible (value ?v1) (group ?g) (id ?id1))
   
   (possible (value ?v2&~?v1) (id ?id1))
   
   (not (possible (value ~?v2&~?v1) (id ?id1)))
   
   (possible (value ?v1) (group ?g) (id ?id2&~?id1))
   
   (possible (value ?v2) (id ?id2))
   
   (not (possible (value ~?v2&~?v1) (id ?id2)))

   (possible (value ?v& ?v1 | ?v2) (group ?g) (id ?id&~?id2&~?id1))

   =>
   
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "Naked Pairs"))))

;;; ############
;;; Hidden Pairs
;;; ############

;;; ****************
;;; hidden-pairs-row
;;; ****************

(defrule hidden-pairs-row

   (priority ?p)

   (technique (name Hidden-Pairs) (priority ?p))
   
   (possible (value ?v1) (row ?r) (column ?c1))
   
   (possible (value ?v2&~?v1) (row ?r) (column ?c1))
      
   (possible (value ?v1) (row ?r) (column ?c2&~?c1))
   
   (possible (value ?v2) (row ?r) (column ?c2))
   
   (not (possible (value ?v1 | ?v2) (row ?r) (column ?c3&~?c2&~?c1)))

   (possible (value ?v&~?v1&~?v2) (row ?r) (column ?c1 | ?c2) (id ?id))

   =>
   
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "Hidden Pairs"))))

;;; *******************
;;; hidden-pairs-column
;;; *******************

(defrule hidden-pairs-column

   (priority ?p)

   (technique (name Hidden-Pairs) (priority ?p))
   
   (possible (value ?v1) (row ?r1) (column ?c))
   
   (possible (value ?v2&~?v1) (row ?r1) (column ?c))
      
   (possible (value ?v1) (row ?r2&~?r1) (column ?c))
   
   (possible (value ?v2) (row ?r2) (column ?c))
   
   (not (possible (value ?v1 | ?v2) (row ?r3&~?r2&~?r1) (column ?c)))

   (possible (value ?v&~?v1&~?v2) (row ?r1 | ?r2) (column ?c) (id ?id))

   =>
   
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "Hidden Pairs"))))


;;; ******************
;;; hidden-pairs-group
;;; ******************

(defrule hidden-pairs-group

   (priority ?p)

   (technique (name Hidden-Pairs) (priority ?p))
   
   (possible (value ?v1) (group ?g) (id ?id1))
   
   (possible (value ?v2&~?v1) (id ?id1))
      
   (possible (value ?v1) (group ?g) (id ?id2&~?id1))
   
   (possible (value ?v2) (id ?id2))
   
   (not (possible (value ?v1 | ?v2) (group ?g) (id ?id3&~?id2&~?id1)))

   (possible (value ?v&~?v1&~?v2) (id ?id&?id1 | ?id2))

   =>
   
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "Hidden Pairs"))))


;;; ######
;;; X Wing
;;; ######

;;; **********
;;; X-Wing-Row
;;; **********

(defrule X-Wing-Row
   
   (priority ?p)

   (technique (name X-Wing) (priority ?p))
   
   (possible (value ?v) (row ?r1) (column ?c1))
   
   (possible (value ?v) (row ?r1) (column ?c2&~?c1))
   
   (not (possible (value ?v) (row ?r1) (column ~?c1&~?c2)))
   
   (possible (value ?v) (row ?r2&~?r1) (column ?c1))
   
   (possible (value ?v) (row ?r2) (column ?c2))
   
   (not (possible (value ?v) (row ?r2) (column ~?c1&~?c2)))
   
   (possible (value ?v) (row ~?r1&~?r2) (column ?c1 | ?c2) (id ?id))
   
   =>
   
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "X Wing"))))

;;; *************
;;; X-Wing-Column
;;; *************

(defrule X-Wing-Column
   
   (priority ?p)

   (technique (name X-Wing) (priority ?p))
   
   (possible (value ?v) (row ?r1) (column ?c1))
   
   (possible (value ?v) (row ?r2&~?r1) (column ?c1))
   
   (not (possible (value ?v) (row ~?r1&~?r2) (column ?c1)))
   
   (possible (value ?v) (row ?r1) (column ?c2&~?c1))
   
   (possible (value ?v) (row ?r2) (column ?c2))
   
   (not (possible (value ?v) (row ~?r1&~?r2) (column ?c2)))
   
   (possible (value ?v) (row ?r1 | ?r2) (column ~?c1&~?c2) (id ?id))
   
   =>
   
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "X Wing"))))

;;; #############
;;; Naked Triples
;;; #############

;;; ****************
;;; generate-triples
;;; ****************

(defrule generate-triples
   
   (declare (salience 10))
   
   (priority ?p)

   (technique (name Naked-Triples) (priority ?p))

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

   (priority ?p)

   (technique (name Naked-Triples) (priority ?p))
   
   (triple ?v1 ?v2 ?v3)
   
   (possible (value ?v1) (row ?r) (id ?id1))
   
   (not (possible (value ~?v1&~?v2&~?v3) (id ?id1)))
   
   (possible (value ?v2) (row ?r) (id ?id2&~?id1))

   (not (possible (value ~?v1&~?v2&~?v3) (id ?id2)))
   
   (possible (value ?v3) (row ?r) (id ?id3&~?id2&~?id1))
   
   (not (possible (value ~?v1&~?v2&~?v3) (id ?id3)))
   
   (possible (value ?v& ?v1 | ?v2 | ?v3) (row ?r) (id ?id&~?id1&~?id2&~?id3))

   =>
   
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "Naked Triples"))))

;;; ********************
;;; naked-triples-column
;;; ********************

(defrule naked-triples-column

   (priority ?p)

   (technique (name Naked-Triples) (priority ?p))
   
   (triple ?v1 ?v2 ?v3)
   
   (possible (value ?v1) (column ?c) (id ?id1))
   
   (not (possible (value ~?v1&~?v2&~?v3) (id ?id1)))
   
   (possible (value ?v2) (column ?c) (id ?id2&~?id1))

   (not (possible (value ~?v1&~?v2&~?v3) (id ?id2)))
   
   (possible (value ?v3) (column ?c) (id ?id3&~?id2&~?id1))
   
   (not (possible (value ~?v1&~?v2&~?v3) (id ?id3)))
   
   (possible (value ?v& ?v1 | ?v2 | ?v3) (column ?c) (id ?id&~?id1&~?id2&~?id3))

   =>
   
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "Naked Triples"))))

;;; *******************
;;; naked-triples-group
;;; *******************

(defrule naked-triples-group

   (priority ?p)

   (technique (name Naked-Triples) (priority ?p))
   
   (triple ?v1 ?v2 ?v3)
   
   (possible (value ?v1) (group ?g) (id ?id1))
   
   (not (possible (value ~?v1&~?v2&~?v3) (id ?id1)))
   
   (possible (value ?v2) (group ?g) (id ?id2&~?id1))

   (not (possible (value ~?v1&~?v2&~?v3) (id ?id2)))
   
   (possible (value ?v3) (group ?g) (id ?id3&~?id2&~?id1))
   
   (not (possible (value ~?v1&~?v2&~?v3) (id ?id3)))
   
   (possible (value ?v& ?v1 | ?v2 | ?v3) (group ?g) (id ?id&~?id1&~?id2&~?id3))

   =>
   
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "Naked Triples"))))

;;; ##############
;;; Hidden Triples
;;; ##############

;;; ******************
;;; hidden-triples-row
;;; ******************

(defrule hidden-triples-row

   (priority ?p)

   (technique (name Hidden-Triples) (priority ?p))
   
   (triple ?v1 ?v2 ?v3)
   
   (possible (value ?v1) (row ?r) (column ?c1))
   
   (possible (value ?v2) (row ?r) (column ?c2&~?c1))
   
   (possible (value ?v3) (row ?r) (column ?c3&~?c2&~?c1))
   
   (not (possible (value ?v1 | ?v2 | ?v3) (row ?r) (column ~?c3&~?c2&~?c1)))
   
   (possible (value ?v&~?v1&~?v2&~?v3) (row ?r) (column ?c1 | ?c2 | ?c3) (id ?id))

   =>
   
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "Hidden Triples"))))

;;; *********************
;;; hidden-triples-column
;;; *********************

(defrule hidden-triples-column

   (priority ?p)

   (technique (name Hidden-Triples) (priority ?p))
   
   (triple ?v1 ?v2 ?v3)
   
   (possible (value ?v1) (row ?r1) (column ?c))
   
   (possible (value ?v2) (row ?r2&~?r1) (column ?c))
   
   (possible (value ?v3) (row ?r3&~?r2&~?r1) (column ?c))
   
   (not (possible (value ?v1 | ?v2 | ?v3) (row ~?r3&~?r2&~?r1) (column ?c)))
   
   (possible (value ?v&~?v1&~?v2&~?v3) (row ?r1 | ?r2 | ?r3) (column ?c) (id ?id))

   =>
   
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "Hidden Triples"))))

;;; ********************
;;; hidden-triples-group
;;; ********************

(defrule hidden-triples-group

   (priority ?p)

   (technique (name Hidden-Triples) (priority ?p))
   
   (triple ?v1 ?v2 ?v3)
   
   (possible (value ?v1) (id ?id1) (group ?g))
   
   (possible (value ?v2) (id ?id2&~?id1) (group ?g))
   
   (possible (value ?v3) (id ?id3&~?id2&~?id1) (group ?g))
   
   (not (possible (value ?v1 | ?v2 | ?v3) (id ~?id3&~?id2&~?id1) (group ?g)))
   
   (possible (value ?v&~?v1&~?v2&~?v3) (id ?id& ?id1 | ?id2 | ?id3))

   =>
   
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "Hidden Triples"))))

;;; #########
;;; Swordfish
;;; #########

;;; *************
;;; swordfish-row
;;; *************

(defrule swordfish-row

   (priority ?p)

   (technique (name Swordfish) (priority ?p))
   
   (triple ?c1 ?c2 ?c3)
   
   (possible (value ?v) (row ?r1) (column ?c1))
   
   (not (possible (value ?v) (row ?r1) (column ~?c1&~?c2&~?c3)))
   
   (possible (value ?v) (row ?r2&~?r1) (column ?c2))

   (not (possible (value ?v) (row ?r2) (column ~?c1&~?c2&~?c3)))
   
   (possible (value ?v) (row ?r3&~?r2&~?r1) (column ?c3))

   (not (possible (value ?v) (row ?r3) (column ~?c1&~?c2&~?c3)))
   
   (possible (value ?v) (row ~?r1&~?r2&~?r3) (column ?c1 | ?c2 | ?c3) (id ?id))

   =>
   
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "Swordfish"))))

;;; ****************
;;; swordfish-column
;;; ****************

(defrule swordfish-column

   (priority ?p)

   (technique (name Swordfish) (priority ?p))
   
   (triple ?r1 ?r2 ?r3)
   
   (possible (value ?v) (row ?r1) (column ?c1))
   
   (not (possible (value ?v) (row ~?r1&~?r2&~?r3) (column ?c1)))
   
   (possible (value ?v) (row ?r2) (column ?c2&~?c1))

   (not (possible (value ?v) (row ~?r1&~?r2&~?r3) (column ?c2)))
   
   (possible (value ?v) (row ?r3) (column ?c3&~?c2&~?c1))

   (not (possible (value ?v) (row ~?r1&~?r2&~?r3) (column ?c3)))
   
   (possible (value ?v) (row ?r1 | ?r2 | ?r3) (column ~?c1&~?c2&~?c3) (id ?id))

   =>
   
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "Swordfish"))))

;;; #######
;;; XY-Wing
;;; #######

;;; *******
;;; XY-Wing
;;; *******
 
(defrule XY-Wing

   (priority ?p)

   (technique (name XY-Wing) (priority ?p))
   
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
   =>
   
   (assert (impossible (id ?id) (value ?z) (priority ?p) (reason "XY-Wing"))))

;;; ######
;;; Colors
;;; ######

;;; *********
;;; color-row
;;; *********

(defrule color-row

   (declare (salience -10))
   
   (phase match)
   
   (priority ?p)

   (technique (name Duplicate-Color | Color-Conjugate-Pair | Multi-Color-Type-1  | Multi-Color-Type-2) 
              (priority ?p))

   (possible (row ?r) (column ?c1) (group ?g1) (id ?id1) (value ?v))

   (possible (row ?r) (column ?c2&~?c1) (group ?g2) (id ?id2) (value ?v))
   
   (not (possible (row ?r) (column ?c3&~?c2&~?c1) (value ?v)))
                     
   (color-pair ?color1 ?color2)
   
   ;; Don't use a color previously used for this value.

   (not (position-value-color (value ?v)
                              (color ?color1 | color2)))
   
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
   
   (priority ?p)

   (technique (name Duplicate-Color | Color-Conjugate-Pair | Multi-Color-Type-1 | Multi-Color-Type-2) 
              (priority ?p))

   (possible (row ?r1) (column ?c) (group ?g1) (id ?id1) (value ?v))

   (possible (row ?r2&~?r1) (column ?c) (group ?g2) (id ?id2) (value ?v))
   
   (not (possible (row ?r3&~?r2&~?r1) (column ?c) (value ?v)))
                     
   (color-pair ?color1 ?color2)
   
   ;; Don't use a color previously used for this value.

   (not (position-value-color (value ?v)
                              (color ?color1 | color2)))
   
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

   (priority ?p)

   (technique (name Duplicate-Color | Color-Conjugate-Pair | Multi-Color-Type-1 | Multi-Color-Type-2) 
              (priority ?p))

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

   (priority ?p)

   (technique (name Duplicate-Color | Color-Conjugate-Pair | Multi-Color-Type-1 | Multi-Color-Type-2) 
              (priority ?p))

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
   
   (priority ?p)

   (technique (name Duplicate-Color | Color-Conjugate-Pair | Multi-Color-Type-1 | Multi-Color-Type-2) 
              (priority ?p))

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

   (priority ?p)

   (technique (name Duplicate-Color) (priority ?p))

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

   =>
   
   (assert (impossible (id ?id1) (value ?v) (priority ?p) (reason "Duplicate Color")))
   
   (assert (impossible (id ?id2) (value ?v) (priority ?p) (reason "Duplicate Color"))))
   
;;; *************************
;;; duplicate-color-in-column
;;; *************************

(defrule duplicate-color-in-column

   (priority ?p)

   (technique (name Duplicate-Color) (priority ?p))

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

   =>
   
   (assert (impossible (id ?id1) (value ?v) (priority ?p) (reason "Duplicate Color")))
   
   (assert (impossible (id ?id2) (value ?v) (priority ?p) (reason "Duplicate Color"))))
   
;;; ************************
;;; duplicate-color-in-group
;;; ************************

(defrule duplicate-color-in-group

   (priority ?p)

   (technique (name Duplicate-Color) (priority ?p))

   (position-value-color (group ?g)
                         (id ?id1)
                         (value ?v)
                         (color ?color))
                                    
   (position-value-color (group ?g)
                         (id ?id2&~?id1)
                         (value ?v)
                         (color ?color))

   =>
   
   (assert (impossible (id ?id1) (value ?v) (priority ?p) (reason "Duplicate Color")))
   
   (assert (impossible (id ?id2) (value ?v) (priority ?p) (reason "Duplicate Color"))))

;;; ####################
;;; Color-Conjugate-Pair
;;; ####################

;;; ********************
;;; color-conjugate-pair
;;; ********************

(defrule color-conjugate-pair

   (priority ?p)

   (technique (name Color-Conjugate-Pair) (priority ?p))

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
                        
   =>
      
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "Color Conjugate Pairs"))))
   
;;; ##################
;;; Multi-Color-Type-1
;;; ##################
   
(defrule multi-color-type-1

   (priority ?p)

   (technique (name Multi-Color-Type-1) (priority ?p))

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
   
   =>

   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "Multi Color Type 1"))))
 
;;; ##################
;;; Multi-Color-Type-2
;;; ##################
   
(defrule multi-color-type-2

   (priority ?p)

   (technique (name Multi-Color-Type-2) (priority ?p))

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
   
   =>
   
   (assert (impossible (id ?id) (value ?v) (priority ?p) (reason "Multi Color Type 2"))))

;;; #############
;;; Forced Chains
;;; #############

;;; ***********
;;; start-chain
;;; ***********

(defrule start-chain

   (declare (salience -10))

   (priority ?p)

   (technique (name Forced-Chain-Convergence | Forced-Chain-XY) (priority ?p))

   (possible (row ?r) (column ?c) (group ?g) (id ?id) (value ?v1))
   
   (possible (id ?id) (value ?v2&~?v1))
   
   (not (possible (id ?id) (value ~?v1&~?v2)))
   
   =>
   
   (assert (chain (start-row ?r)
                  (start-column ?c)
                  (start-value ?v1)
                  (row ?r)
                  (column ?c)
                  (group ?g)
                  (id ?id)
                  (value ?v1)))
                  
   (assert (chain (start-row ?r)
                  (start-column ?c)
                  (start-value ?v2)
                  (row ?r)
                  (column ?c)
                  (group ?g)
                  (id ?id)
                  (value ?v2))))  

;;; ******************
;;; continue-chain-row
;;; ******************

(defrule continue-chain-row

   (declare (salience -10))

   (priority ?p)

   (technique (name Forced-Chain-Convergence | Forced-Chain-XY) (priority ?p))

   ?f <- (chain (row ?r) (column ?c1) (value ?v1))
          
   (possible (row ?r) (column ?c2&~?c1) (value ?v1))
   
   (possible (row ?r) (column ?c2) (group ?g) (id ?id) (value ?v2&~?v1))
   
   (not (possible (row ?r) (column ?c2) (value ?v3&~?v2&~?v1)))
                 
   =>
   
   (duplicate ?f (column ?c2)
                 (group ?g)
                 (id ?id)
                 (value ?v2)))

;;; *********************
;;; continue-chain-column
;;; *********************

(defrule continue-chain-column

   (declare (salience -10))

   (priority ?p)

   (technique (name Forced-Chain-Convergence | Forced-Chain-XY) (priority ?p))

   ?f <- (chain (row ?r1) (column ?c) (value ?v1))
          
   (possible (row ?r2&~?r1) (column ?c) (value ?v1))
   
   (possible (row ?r2) (column ?c) (group ?g) (id ?id) (value ?v2&~?v1))
   
   (not (possible (row ?r2) (column ?c) (value ?v3&~?v2&~?v1)))
                 
   =>
   
   (duplicate ?f (row ?r2)
                 (group ?g)
                 (id ?id)
                 (value ?v2)))

;;; ********************
;;; continue-chain-group
;;; ********************

(defrule continue-chain-group

   (declare (salience -10))

   (priority ?p)

   (technique (name Forced-Chain-Convergence | Forced-Chain-XY) (priority ?p))

   ?f <- (chain (group ?g) (id ?id1) (value ?v1))
          
   (possible (row ?r) (column ?c) (group ?g) (id ?id2&~?id1) (value ?v1))
   
   (possible (id ?id2) (value ?v2&~?v1))
   
   (not (possible (id ?id2) (value ?v3&~?v2&~?v1)))
                 
   =>
   
   (duplicate ?f (row ?r)
                 (column ?c)
                 (id ?id2)
                 (value ?v2)))

;;; ************************
;;; forced-chain-convergence
;;; ************************

(defrule forced-chain-convergence

   (priority ?p)

   (technique (name Forced-Chain-Convergence) (priority ?p))

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

   =>
   
   (assert (impossible (id ?id) (value ?v3) (priority ?p) (reason "Forced Chain Convergence"))))

;;; ***************
;;; forced-chain-XY
;;; ***************

(defrule forced-chain-XY

   (priority ?p)

   (technique (name Forced-Chain-XY) (priority ?p))

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

   =>
   
   (assert (impossible (id ?id) (value ?v1) (priority ?p) (reason "Forced Chain XY"))))
   
;;; ################
;;; Unique-Rectangle
;;; ################

;;; ********************
;;; Unique-Rectangle-Row
;;; ********************

(defrule Unique-Rectangle-Row
   
   (priority ?p)

   (technique (name Unique-Rectangle) (priority ?p))
   
   (possible (value ?v1) (group ?g1) (row ?r1) (column ?c1))

   (possible (value ?v2&~?v1) (group ?g1) (row ?r1) (column ?c1))

   (not (possible (value ~?v2&~?v1) (row ?r1) (column ?c1)))
   
   (possible (value ?v1) (group ?g1) (row ?r1) (column ?c2&~?c1))

   (possible (value ?v2&~?v1) (group ?g1) (row ?r1) (column ?c2&~?c1))

   (not (possible (value ~?v2&~?v1) (row ?r1) (column ?c2)))
   
   (possible (value ?v1) (group ?g2&~?g1) (row ?r2) (column ?c1))

   (possible (value ?v2) (group ?g2) (row ?r2) (column ?c1))
   
   (not (possible (value ~?v2&~?v1) (group ?g2) (row ?r2) (column ?c1)))  
   
   (possible (value ?v1) (id ?id1) (group ?g2) (row ?r2) (column ?c2))

   (possible (value ?v2) (id ?id2) (group ?g2) (row ?r2) (column ?c2))
   
   (possible (value ~?v2&~?v1) (group ?g2) (row ?r2) (column ?c2)) 
   
   =>
   
   (assert (impossible (id ?id1) (value ?v1) (priority ?p) (reason "Unique Rectangle")))

   (assert (impossible (id ?id2) (value ?v2) (priority ?p) (reason "Unique Rectangle"))))
   
;;; ***********************
;;; Unique-Rectangle-Column
;;; ***********************

(defrule Unique-Rectangle-Column
   
   (priority ?p)

   (technique (name Unique-Rectangle) (priority ?p))
   
   (possible (value ?v1) (group ?g1) (row ?r1) (column ?c1))

   (possible (value ?v2&~?v1) (group ?g1) (row ?r1) (column ?c1))

   (not (possible (value ~?v2&~?v1) (row ?r1) (column ?c1)))
   
   (possible (value ?v1) (group ?g1) (row ?r2&~?r1) (column ?c1))

   (possible (value ?v2&~?v1) (group ?g1) (row ?r2&~?r1) (column ?c1))

   (not (possible (value ~?v2&~?v1) (row ?r2) (column ?c1)))
   
   (possible (value ?v1) (group ?g2&~?g1) (row ?r1) (column ?c2))

   (possible (value ?v2) (group ?g2) (row ?r1) (column ?c2))
   
   (not (possible (value ~?v2&~?v1) (group ?g2) (row ?r1) (column ?c2)))  
   
   (possible (value ?v1) (id ?id1) (group ?g2) (row ?r2) (column ?c2))

   (possible (value ?v2) (id ?id2) (group ?g2) (row ?r2) (column ?c2))
   
   (possible (value ~?v2&~?v1) (group ?g2) (row ?r2) (column ?c2)) 
   
   =>
   
   (assert (impossible (id ?id1) (value ?v1) (priority ?p) (reason "Unique Rectangle")))

   (assert (impossible (id ?id2) (value ?v2) (priority ?p) (reason "Unique Rectangle"))))

