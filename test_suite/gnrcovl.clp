(defmethod + ((?a SYMBOL) $?any)
  (bind ?i 1)
  (bind ?len (length ?any))
  (while (<= ?i ?len) do
     (bind ?a (sym-cat ?a (nth ?i ?any)))
     (bind ?i (+ ?i 1)))
  ?a)

(defmethod sym-cat (?a ?b)
  (sym-cat my- ?a ?b))

(defmethod + ((?a STRING) $?any)
  (bind ?i 1)
  (bind ?len (length ?any))
  (while (<= ?i ?len) do
     (bind ?a (str-cat ?a (nth ?i ?any)))
     (bind ?i (+ ?i 1)))
  ?a)

(deffunction alt-str-cat ($?any)
  (str-cat $?any))
  
(defmethod + ((?a MULTIFIELD) $?any)
  (mv-append ?a ?any))

(defglobal ?*success* = TRUE)

(deffunction print-result (?flag ?test-no)
  (if ?flag then
     (printout t "OVERLOAD TEST #" ?test-no " OK." crlf)
   else
     (printout t "OVERLOAD TEST #" ?test-no " BAD." crlf)
     (bind ?*success* FALSE)))

(deffunction testit ()
  (print-result (eq (sym-cat a b c) abc) 1)
  (print-result (eq (sym-cat a b) my-ab) 2)
  (print-result (eq (+ a b c d e f) abcdef) 3)
  (print-result (eq (+ "a" "bc" "d" "ef") (alt-str-cat "a" "bc" "d" "ef") "abcdef") 4)
  (print-result 
    (eq (+ (mv-append a) (mv-append b c)) (mv-append a b c)) 5)
  (if ?*success* then
     (printout t "No errors detected." crlf)
   else
     (printout t "Errors detected." crlf)))
