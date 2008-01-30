(defmethod + ((?arg1 INTEGER (> ?arg1 0)) (?arg2 INTEGER (> ?arg2 0)))
  (printout t "This should be the 1st of 7 methods." crlf)
  (if (next-methodp) then
     (call-next-method)))

(defmethod + ((?arg1 INTEGER (> ?arg1 0)) (?arg2 NUMBER))
  (printout t "This should be the 2nd of 7 methods." crlf)
  (if (next-methodp) then
     (call-next-method)))

(defmethod + ((?arg1 INTEGER) (?arg2 INTEGER))
  (printout t "This should be the 3rd of 7 methods." crlf)
  (if (next-methodp) then
     (call-next-method)))

(defmethod + ((?arg1 INTEGER) (?arg2 NUMBER))
  (printout t "This should be the 4th of 7 methods." crlf)
  (if (next-methodp) then
     (call-next-method)))

(defmethod + ((?arg1 NUMBER) (?arg2 INTEGER (> ?arg2 0)))
  (printout t "This should be the 5th of 7 methods." crlf)
  (if (next-methodp) then
     (call-next-method)))

(defmethod + ((?arg1 NUMBER) (?arg2 INTEGER))
  (printout t "This should be the 6th of 7 methods." crlf)
  (if (next-methodp) then
     (call-next-method)))

(defmethod + ((?arg1 NUMBER) (?arg2 NUMBER))
  (printout t "This should be the 7th of 7 methods." crlf)
  (if (next-methodp) then
     (call-next-method)))

(defmethod t1 ((?a INTEGER LEXEME))
  (printout t "This should be the 1st of 2 methods." crlf)
  (if (next-methodp) then
     (call-next-method)))

(defmethod t1 ((?a NUMBER STRING))
  (printout t "This should be the 2nd of 2 methods." crlf)
  (if (next-methodp) then
     (call-next-method)))

(defmethod t2 ((?a LEXEME))
  (printout t "This should be the 1st of 2 methods." crlf)
  (if (next-methodp) then
     (call-next-method)))

(defmethod t2 ((?a MULTIFIELD STRING))
  (printout t "This should be the 2nd of 2 methods." crlf)
  (if (next-methodp) then
     (call-next-method)))

(defmethod t3 ((?a INTEGER LEXEME))
  (printout t "This should be the 1st of 2 methods." crlf)
  (if (next-methodp) then
     (call-next-method)))

(defmethod t3 ((?a STRING NUMBER))
  (printout t "This should be the 2nd of 2 methods." crlf)
  (if (next-methodp) then
     (call-next-method)))

(defgeneric mv-slot-replace)
(defgeneric class-slots)
(defmethod mv-slot-replace ((?a INSTANCE-ADDRESS)))
(defmethod mv-slot-replace ((?a PRIMITIVE)))
(defmethod mv-slot-replace ((?a INSTANCE SYMBOL) (?b SYMBOL) (?c NUMBER) (?d NUMBER) ?e))
(defmethod class-slots ((?a SYMBOL) (?b LEXEME)))
(defmethod class-slots ((?a SYMBOL) ($?b SYMBOL)))

(deffunction testit ()
  (printout t crlf "OUTPUT OF TEST #1 FOLLOWS..." crlf crlf)
  (if (<> (+ 2 2) 4) then
     (printout t "RESULT OF TEST #1 BAD." crlf))
  (printout t crlf "OUTPUT OF TEST #2 FOLLOWS..." crlf crlf)
  (t1 "abc")
  (printout t crlf "OUTPUT OF TEST #3 FOLLOWS..." crlf crlf)
  (t2 "abc")
  (printout t crlf "OUTPUT OF TEST #4 FOLLOWS..." crlf crlf)
  (t3 "abc")
  (list-defmethods))

