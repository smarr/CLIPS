(defmethod n! 1 ((?a INTEGER (= ?a 0)))
  1)

(defmethod n! 2 ((?a INTEGER (> ?a 0)))
  (* ?a (n! (- ?a 1))))

(defmethod n! 3 ($?any)
  (printout t "Illegal argument(s) for factorial." crlf))

(defmethod + ((?a INTEGER) $?any)
  (bind ?r (call-next-method))
  (if (> ?r 5) then
     (- ?r 2)
   else
     (- ?r 1)))

(defmethod + ((?a INTEGER) ($?any INTEGER))
  (bind ?r (call-next-method))
  (if (> ?r 5) then
     (- ?r 3)
   else
     (- ?r 2)))

(defmethod + ((?a INTEGER) ($?any INTEGER (> ?current-argument 5)))
  (bind ?r (call-next-method))
  (if (> ?r 5) then
     (- ?r 4)
   else
     (- ?r 3)))

(defmethod + (($?any STRING))
  (str-cat $?any))
  
(defmethod + (($?any SYMBOL))
  (sym-cat $?any))

(defmethod + (($?any MULTIFIELD))
  ?any)
  
(defmethod next-test ()
  (next-methodp))

(defmethod next-test ($?any)
  (next-methodp))

(defclass MY-CLASS (is-a USER) (role concrete))

(defmethod lots-of-arguments 
  ((?a INTEGER) (?b STRING) ?c ?d ?e ?f (?g MY-CLASS) $?more)
  (str-implode (mv-append ?a ?b ?c ?d ?e ?f ?g ?more)))

(defmethod foo ()
   (bind ?b 10)
   (call-next-method)
 ?b)

(defmethod foo ((?a INSTANCE) (?b INSTANCE-NAME) (?c INSTANCE-ADDRESS))
  42)

(defmethod foo ($?any)
  (bind ?b 20))

(defclass A (is-a USER) (role concrete))

(defglobal ?*success* = TRUE)
(defglobal ?*sevar* = (mv-append))

(deffunction print-result (?flag ?test-no)
  (if ?flag then
     (printout t "EXECUTION TEST #" ?test-no " OK." crlf)
   else
     (printout t "EXECUTION TEST #" ?test-no " BAD." crlf)
     (bind ?*success* FALSE)))

(defmethod side-effects ((?a (bind ?*sevar* (mv-append ?*sevar* 1))))
   (bind ?*sevar* (mv-append ?*sevar* 3))
   (call-next-method)
   ?*sevar*)

(defmethod side-effects ((?a (bind ?*sevar* (mv-append ?*sevar* 2))))
   (bind ?*sevar* (mv-append ?*sevar* 4)))

(defmethod - ((?a INTEGER) (?b INTEGER) ($?c INTEGER))
   (override-next-method (* ?a 2) (* ?b 3) (expand$ ?c)))

(defmethod - ((?a INTEGER) (?b INTEGER))
  (override-next-method ?a ?b 1))

(deffunction testit ()
  (make-instance [a] of A)
  (print-result (= (+ 6 6) 3) 1)
  (print-result (= (+ 2 4) 2) 2)
  (print-result (= (+ 1 25 36) 53) 3)
  (print-result (= (+ -1 -25 -36) -65) 4)
  (print-result (= (+ 1.0 2.0 3.0) 6.0) 5)
  (print-result (= (n! 0) 1) 6)
  (print-result (= (n! 1) 1) 7)
  (print-result (= (n! 5) 120) 8)  
  (print-result (eq (type 1) INTEGER) 9)
  (print-result (eq (type 1.0) FLOAT) 10)
  (print-result 
    (eq (type (make-instance my-instance of MY-CLASS)) MY-CLASS) 11)
  (print-result (next-test) 12)
  (print-result (not (next-test abc)) 13)
  (print-result 
    (eq (lots-of-arguments 1 "a" b c d 4 [my-instance] abc def) 
        "1 \"a\" b c d 4 [my-instance] abc def") 14)
  (print-result (eq (+ a b c d e f) abcdef) 15)
  (print-result (eq (+ "a b c" "d e f") "a b cd e f") 16)
  (print-result (eq (+ "a b c" "d e f") "a b cd e f") 17)
  (print-result (eq (+ (mv-append a b c) (mv-append) (mv-append d e f)) (mv-append a b c d e f)) 18)
  (print-result (= (foo) 10) 19)
  (print-result (= (foo [a] [a] (instance-address [a])) 42) 20)
  (print-result (eq (side-effects 1) (mv-append 1 3 2 4)) 21)
  (print-result (= (- 1 2) -5) 22)
  (print-result (= (call-specific-method - 2 1 2) -4) 23)
  (print-result (= (call-specific-method - 1 1 2) -1) 24)
  (print-result (= (- 1 2 0) -4) 25)
  (print-result (= (- 1.0 2) -1.0) 26)
  (if ?*success* then
     (printout t "No errors detected." crlf)
   else
     (printout t "Errors detected." crlf)))
