(deftemplate fact
   (field foo)
   (multifield bar)
)

(deffacts test-facts
   (fact (bar 1 2 3 4))
   (flat-fact a b c d e)
)

(defclass SEQOP-TEST-CLASS (is-a USER)
   (role concrete)
   (multislot woz (create-accessor read-write) (default watch dog))
)

(definstances seqop-tests
   (s1 of SEQOP-TEST-CLASS)
)

(defmethod test-arg ($?a)
  TRUE
)

(defmethod test-arg ((?a MULTIFIELD))
  FALSE
)

(defrule lhs-seqop-success
   (declare (salience 10))
   ?f<-(fact (foo ?single) (bar $?multi))
   ?g<-(flat-fact ?single2 $?multi2)
   (test (and (test-arg $?multi) (test-arg $?multi2)))
=>
   (retract ?f ?g)
   (bind ?a (mv-append abc def ghi))
   (printout t ?a " --> " $?a " ; " (expand$ ?a) crlf)
   (printout t "Success:" crlf ?multi  " --> " $?multi  crlf
                               ?multi2 " --> " $?multi2 crlf)
)

(defrule lhs-seqop-failure
   ?f<-(fact)
   ?g<-(flat-fact ? $?)
=>
   (retract ?f ?g)
   (printout t "Failure!" crlf)
)

(deffunction dfnx-seqop-test (?a $?b)
   (bind ?a (mv-append abc def ghi))
   (printout t ?a " --> " $?a " ; " (expand$ ?a) crlf)
   (printout t ?b " --> " $?b crlf)
)

(defmessage-handler SEQOP-TEST-CLASS seqop-test (?a $?b)
   (bind ?a (mv-append abc def ghi))
   (printout t ?a " --> " $?a " ; " (expand$ ?a) crlf)
   (printout t ?b " --> " $?b crlf)
   (printout t ?self:woz " --> " (expand$ ?self:woz) crlf)
   (bind ?c (make-instance (gensym*) of SEQOP-TEST-CLASS
              (woz ?self:woz $?a ?self:woz)))
   (printout t (send ?c get-woz) crlf)
)

(defmethod + (($?b STRING))
   (bind ?a (mv-append abc def ghi))
   (printout t ?a " --> " $?a " ; " (expand$ ?a) crlf)
   (printout t ?b " --> " $?b crlf)
   (str-cat $?b)
)
   
   
