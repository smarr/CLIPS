(defclass T (is-a OBJECT))
(defclass A (is-a USER))
(defclass B (is-a USER))
(defclass C (is-a A B))
(defclass D (is-a B A))
(defclass E (is-a C A))
(defclass F (is-a C B))
(defclass G (is-a A))
(defclass H (is-a B))
(defclass I (is-a G H A B))
(defclass J (is-a G H A))
(defclass K (is-a A B T))
(defclass L (is-a G H))
(defclass M (is-a L A B))
(defclass N (is-a C))
(defclass O (is-a D))
(defclass P (is-a USER))
(defclass Q (is-a USER))
(defclass R (is-a P Q))
(defclass S (is-a C R))
(defclass U (is-a C G))
(defclass V (is-a G C))

(deffunction mult-error-test (?test-num ?superstr)
  (if (build (str-cat "(defclass JUNK (is-a " ?superstr "))")) then
     (printout t "INHERITANCE ERROR TEST #" ?test-num " BAD." crlf)
   else
     (printout t "INHERITANCE ERROR TEST #" ?test-num " OK." crlf)))

(deffunction testit ()
  (printout t (class-superclasses T inherit) crlf
              (class-superclasses A inherit) crlf
              (class-superclasses B inherit) crlf
              (class-superclasses C inherit) crlf
              (class-superclasses D inherit) crlf
              (class-superclasses E inherit) crlf
              (class-superclasses F inherit) crlf
              (class-superclasses G inherit) crlf
              (class-superclasses H inherit) crlf
              (class-superclasses I inherit) crlf
              (class-superclasses J inherit) crlf
              (class-superclasses K inherit) crlf
              (class-superclasses L inherit) crlf
              (class-superclasses M inherit) crlf
              (class-superclasses N inherit) crlf
              (class-superclasses O inherit) crlf
              (class-superclasses P inherit) crlf
              (class-superclasses Q inherit) crlf
              (class-superclasses R inherit) crlf
              (class-superclasses S inherit) crlf
              (class-superclasses U inherit) crlf
              (class-superclasses V inherit) crlf
   )
  (printout t crlf "THE REST SHOULD BE ERRORS." crlf crlf)
  (mult-error-test 1 "C D")
  (mult-error-test 2 "A C")
  (mult-error-test 3 "N O"))

