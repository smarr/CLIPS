(deffacts factoids
  ; a factoids
  (factoid a 1 2)
  (factoid a 1 2 3)
  (factoid a 1 2 3 4)
  ; b factoids
  (factoid b 1 2 1 1 2 2 1 2)
  ; c factoids
  (factoid c 1 1 2 1 2)
  ; d factoids
  (factoid d)
  (factoid d 1 2 1 2)
  (factoid d 1 1)
  ; e factoids
  (factoid e 1 2 1 2 3 4 1 2 3))

(defrule mfvtest1
  (declare (salience 99))
  (factoid a $?x&:(> (length$ ?x) 3))
  =>
  (assert (answer (gensym) (format nil "01: (%s)" (implode$ ?x)))))
  
(defrule mfvtest2
  (declare (salience 96))
  (factoid a $?x&:(< (length$ ?x) 3)|:(> (length$ ?x) 3))
  =>
  (assert (answer (gensym) (format nil "02: (%s)" (implode$ ?x)))))

(defrule mfvtest3
  (declare (salience 93))
  (factoid a $?x&:(> (length$ ?x) 1)&:(< (length$ ?x) 3))
  =>
  (assert (answer (gensym) (format nil "03: (%s)" (implode$ ?x)))))

(defrule mfvtest4
  (declare (salience 90))
  (factoid b $?x&:(> (length$ ?x) 3) 
             $?y&:(< (length$ ?y) 3) 
             $?z&$?x|$?y 
             $?w&~$?x&~$?y)
  =>
  (assert (answer (gensym) (format nil "04: (%s) (%s) (%s) (%s)" 
                              (implode$ ?x) (implode$ ?y) 
                              (implode$ ?z) (implode$ ?w)))))

(defrule mfvtest5
  (declare (salience 87))
  (factoid b $?x $?y&=(create$ 1 2) $?z)
  =>
  (assert (answer (gensym) (format nil "05: (%s) (%s) (%s)" 
                              (implode$ ?x) (implode$ ?y) 
                              (implode$ ?z)))))

(defrule mfvtest6
  (declare (salience 84))
  (not (factoid a $?x&:(member$ 9 ?x)))
  =>
  (assert (answer (gensym) (format nil "06: Passed"))))

(defrule mfvtest7
  (declare (salience 81))
  (factoid c $?x&:(= (length$ ?x) 1) $?y&~$?x $?z $?w&$?y|$?x)
  =>
  (assert (answer (gensym) (format nil "07: (%s) (%s) (%s) (%s)" 
                              (implode$ ?x) (implode$ ?y) 
                              (implode$ ?z) (implode$ ?w)))))

(defrule mfvtest8
  (declare (salience 78))
  (factoid d $?x ~$?x)
  =>
  (assert (answer (gensym) (format nil "08: (%s)" (implode$ ?x)))))

(defrule mfvtest9
  (declare (salience 75))
  (factoid d $?x)
  (factoid d $?y&~$?x)
  =>
  (assert (answer (gensym) (format nil "09: (%s) (%s)" 
                              (implode$ ?x) (implode$ ?y)))))

(defrule mfvtest10
  (declare (salience 72))
  (factoid a $?y)
  (factoid e $?x $?y $?z)
  =>
  (assert (answer (gensym) (format nil "10: (%s) (%s) (%s)" 
                              (implode$ ?x) (implode$ ?y) 
                              (implode$ ?z)))))

(defrule print-answer
   ?f <- (answer ? ?a)
   (not (answer ? ?b&:(> (str-compare ?a ?b) 0)))
   =>
   (retract ?f)
   (printout t ?a crlf))
