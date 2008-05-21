;;; ############
;;; OUTPUT RULES
;;; ############

;;; *************
;;; print-initial
;;; *************

(defrule print-initial

   (declare (salience 10))
   
   (phase initial-output)

   =>)

;;; ***********
;;; print-final
;;; ***********

(defrule print-final

   (declare (salience 10))
   
   (phase final-output)

   =>)

;;; **************************
;;; print-position-value-found
;;; **************************

(defrule print-position-value-found

   (phase initial-output | final-output)
   
   (print-position ?r ?c)
   
   (possible (row ?r) (column ?c) (value ?v))
   
   (not (possible (row ?r) (column ?c) (value ~?v)))
   
   (size ?s)
   
   =>
   
   (assert (position-printed ?r ?c)))
   
;;; ******************************
;;; print-position-value-not-found
;;; ******************************

(defrule print-position-value-not-found

   (declare (salience -5))

   (phase initial-output | final-output)
   
   (print-position ?r ?c)
   
   (not (position-printed ?r ?c))
             
   (size ?s)
   
   =>)
   
;;; ********************
;;; next-position-column
;;; ********************

(defrule next-position-column

   (declare (salience -10))

   (phase initial-output | final-output)
   
   (size ?s)
   
   ?f <- (print-position ?r ?c&~=(* ?s ?s))
      
   =>
   
   (retract ?f)
   
   (assert (print-position ?r (+ 1 ?c))))

;;; *****************
;;; next-position-row
;;; *****************
   
(defrule next-position-row

   (declare (salience -10))

   (phase initial-output | final-output)
   
   (size ?s)
   
   ?f <- (print-position ?r&~=(* ?s ?s) =(* ?s ?s))
      
   =>
      
   (retract ?f)
   
   (assert (print-position (+ 1 ?r) 1)))
   
;;; ************************
;;; output-done-rule-listing
;;; ************************

(defrule output-done-rule-listing

   (declare (salience -10))

   ?f1 <- (phase final-output)
   
   (size ?s)

   ?f2 <- (print-position =(* ?s ?s) =(* ?s ?s))
   
   (exists (technique-employed))
      
   =>
   
   (retract ?f1 ?f2)
   
   (assert (phase list-rules)))
   
;;; ***************************
;;; output-done-no-rule-listing
;;; ***************************

(defrule output-done-no-rule-listing

   (declare (salience -10))

   (phase final-output)
   
   (size ?s)
   
   ?f <- (print-position =(* ?s ?s) =(* ?s ?s))
   
   (not (technique-employed))
      
   =>
   
   (retract ?f))

;;; *******************
;;; initial-output-done
;;; *******************

(defrule initial-output-done

   (declare (salience -10))

   (phase initial-output)
   
   (size ?s)
   
   ?f1 <- (print-position =(* ?s ?s) =(* ?s ?s))
      
   =>
   
   (retract ?f1))
      
;;; *********
;;; list-rule
;;; *********

(defrule list-rule

   (phase list-rules)
      
   ?f <- (technique-employed (priority ?p) (reason ?reason))
   
   (not (technique-employed (priority ?p2&:(< ?p2 ?p))))
      
   =>
   
   (retract ?f))
    
;;; **************
;;; list-rule-done
;;; **************

(defrule list-rule-done

   (declare (salience -10))

   ?f <- (phase list-rules)
            
   =>
   
   (retract ?f))