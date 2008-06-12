;;; ############
;;; OUTPUT RULES
;;; ############

(defglobal ?*output* = t)

;;; *************
;;; print-initial
;;; *************

(defrule print-initial

   (declare (salience 10))
   
   (phase initial-output)

   =>
   
   (printout ?*output* crlf "The puzzle is: " crlf crlf "   "))

;;; ***********
;;; print-final
;;; ***********

(defrule print-final

   (declare (salience 10))
   
   (phase final-output)

   =>
   
   (printout ?*output* crlf "The solution is: " crlf crlf "   "))

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
   
   (assert (position-printed ?r ?c))
   
   (if (> ?s 3)
      then
      (if (> ?v 9)
         then
         (printout ?*output* ?v)
         else
         (printout ?*output* " " ?v))
      else
      (printout ?*output* ?v)))
   
;;; ******************************
;;; print-position-value-not-found
;;; ******************************

(defrule print-position-value-not-found

   (declare (salience -5))

   (phase initial-output | final-output)
   
   (print-position ?r ?c)
   
   (not (position-printed ?r ?c))
             
   (size ?s)
   
   =>
   
   (assert (position-printed ?r ?c))
   
   (if (> ?s 3)
      then
      (printout ?*output* " *")
      else
      (printout ?*output* *)))
   
;;; ********************
;;; next-position-column
;;; ********************

(defrule next-position-column

   (declare (salience -10))

   (phase initial-output | final-output)
   
   (size ?s)
   
   ?f1 <- (print-position ?r ?c&~=(* ?s ?s))
   
   ?f2 <- (position-printed ?r ?c)
      
   =>

   (if (= (mod ?c ?s) 0)
      then
      (printout ?*output* "  ")
      else
      (printout ?*output* " "))
   
   (retract ?f1 ?f2)
   
   (assert (print-position ?r (+ 1 ?c))))

;;; *****************
;;; next-position-row
;;; *****************
   
(defrule next-position-row

   (declare (salience -10))

   (phase initial-output | final-output)
   
   (size ?s)
   
   ?f1 <- (print-position ?r&~=(* ?s ?s) ?c&=(* ?s ?s))
      
   ?f2 <- (position-printed ?r ?c)
   
   =>

   (if (= (mod ?r ?s) 0)
      then
      (printout ?*output* crlf crlf "   ")
      else
      (printout ?*output* crlf "   "))
      
   (retract ?f1 ?f2)
   
   (assert (print-position (+ 1 ?r) 1)))
   
;;; ************************
;;; output-done-rule-listing
;;; ************************

(defrule output-done-rule-listing

   (declare (salience -10))

   ?f1 <- (phase final-output)
   
   (size ?s)

   ?f2 <- (print-position ?r&=(* ?s ?s) ?c&=(* ?s ?s))
   
   ?f3 <- (position-printed ?r ?c)
   
   (exists (technique-employed))
      
   =>
   
   (printout ?*output* crlf crlf "Rules used:" crlf crlf)
   
   (retract ?f1 ?f2 ?f3)
   
   (assert (phase list-rules)))
   
;;; ***************************
;;; output-done-no-rule-listing
;;; ***************************

(defrule output-done-no-rule-listing

   (declare (salience -10))

   (phase final-output)
   
   (size ?s)
   
   ?f1 <- (print-position ?r&=(* ?s ?s) ?c&=(* ?s ?s))
   
   ?f2 <- (position-printed ?r ?c)
   
   (not (technique-employed))
      
   =>
   
   (printout ?*output* crlf)
   
   (retract ?f1 ?f2))

;;; *******************
;;; initial-output-done
;;; *******************

(defrule initial-output-done

   (declare (salience -10))

   (phase initial-output)
   
   (size ?s)
   
   ?f1 <- (print-position ?r&=(* ?s ?s) ?c&=(* ?s ?s))
   
   ?f2 <- (position-printed ?r ?c)
         
   =>
   
   (printout ?*output* crlf)
   
   (retract ?f1 ?f2))
      
;;; *********
;;; list-rule
;;; *********

(defrule list-rule

   (phase list-rules)
      
   ?f <- (technique-employed (rank ?p) (reason ?reason))
   
   (not (technique-employed (rank ?p2&:(< ?p2 ?p))))
      
   =>
   
   (printout ?*output* "   " ?reason crlf)
   
   (retract ?f))
    
;;; **************
;;; list-rule-done
;;; **************

(defrule list-rule-done

   (declare (salience -10))

   ?f <- (phase list-rules)
            
   =>
   
   (printout ?*output* crlf)
   
   (retract ?f))