(deffunction print-args (?a ?b $?c)
   (printout t ?a " " ?b " and " (length ?c) " extras: " ?c crlf))
(print-args 1 2)
(print-args 1 2 3 4)
(print-args 1 2 (mv-append a b c) (mv-append d e f) 1 2 3 4)
(deffunction test-return ()
   (+ (eval "(gensym)") 2)
   TRUE)
(test-return)
(deffunction test-return ()
  1 2 3 4)
(test-return)
(deffunction factorial (?a)
   (if (or (not (integerp ?a)) (< ?a 0)) then
      (printout t "Factorial error!" crlf)
    else
      (if (= ?a 0) then
         1
       else
         (* ?a (factorial (- ?a 1))))))
(factorial 5)
(* 5 4 3 2)
(factorial 0)
(factorial 1)
(factorial abc)

(defglobal ?*cnt* = 0)

(deffunction foo ())
(deffunction bar ()
   (if (< ?*cnt* 10) then
      (bind ?*cnt* (+ ?*cnt* 1))
      (foo)))
(deffunction foo ()
  (bar))
(foo)
(deffunction wildcard-test (?a $?rest)
  (printout t ?rest crlf)
  (bind ?rest 34)
  (printout t ?rest crlf))
(wildcard-test 1 2 3 4 5 6)
