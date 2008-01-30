(defglobal ?*q* = (create$ a "b" c)
           ?*r* = 3
           ?*s* = (create$ d e "f")
           ?*x* = s
           ?*y* = 3.5
           ?*z* = (create$ k 1 9.4))

(defclass WHIZ
  (is-a USER)
  (role concrete)
  (multislot q (visibility public)
     (create-accessor read-write) (default (create$ a "b" c))))
   
(defmessage-handler WHIZ put-q around ($?val)
  (bind ?self:q ?val))

(defclass HERK
  (is-a USER)
  (role concrete)
  (slot r (visibility public) (create-accessor read-write) (default 3)))

(defclass KLOD
  (is-a WHIZ HERK)
  (role concrete)
  (slot y (visibility public) (create-accessor read-write) (default 3.5)))

(defclass QUIG  
   (is-a USER)
  (role concrete)
  (multislot s (visibility public) 
     (create-accessor read-write) (default (create$ d e "f")))
  (slot x (visibility public) (create-accessor read-write) (default s)))

(defclass THINGO 
  (is-a KLOD QUIG)
  (role concrete)
  (multislot z (visibility public) 
     (create-accessor read-write) (default (create$ k 1 9.4))))

(defclass PHLEGM (is-a THINGO) (role concrete))

(defmessage-handler THINGO quoxnar ()
   (printout t (str-implode ?self:q) " " (str-cat ?self:r) " " 
               (str-implode ?self:s) " " (str-cat ?self:x) " "
               (str-cat ?self:y) " " (str-implode ?self:z) crlf))

(defmessage-handler THINGO fribban ()
   (send ?self put-r ?self:x)
   (send ?self put-x 3.5)
   (send ?self put-y ?*r*)
   (send ?self put-q ?self:s)
   (send ?self put-s (create$ k 1 9.4))
   (send ?self put-z ?*q*)
   (printout t (str-implode ?self:q) " " (str-cat ?self:r) " " 
               (str-implode ?self:s) " " (str-cat ?self:x) " "
               (str-cat ?self:y) " " (str-implode ?self:z) crlf))

(defmessage-handler PHLEGM get-x around ()
   (printout t "This message should appear first" crlf)
   (bind ?return (call-next-handler))
   (printout t "This message should appear forth" crlf)
   ?return
   )

(defmessage-handler PHLEGM get-x before ()
   (printout t "This message should appear second" crlf)
   )

(defmessage-handler PHLEGM get-x after ()
   (printout t "This message should appear third" crlf)
   )

(defmessage-handler PHLEGM put-y around (?x)
   (printout t "This message should appear fifth" crlf)
   (call-next-handler)
   (printout t "This message should appear eighth" crlf)
   )

(defmessage-handler PHLEGM put-y before (?x)
   (printout t "This message should appear sixth" crlf)
   )

(defmessage-handler PHLEGM put-y after (?x)
   (printout t "This message should appear seventh" crlf)
   )

(defmessage-handler PHLEGM get-y around ()
   (call-next-handler)
   around-override
   )

(defmessage-handler PHLEGM get-y before ()
   blah
   )

(defmessage-handler PHLEGM get-y after ()
   uhuh
   )

(defmessage-handler PHLEGM get-r before ()
   10
   )

(defmessage-handler PHLEGM get-r after ()
   15
   )

(defclass BOGUS (is-a USER) (role concrete))

(defmessage-handler BOGUS bad-message-1 ()
  (integer (eval "(gensym)"))
  (printout t "ERROR: This message should NOT be output!" crlf))

(defmessage-handler BOGUS bad-message-2 ()
  (integer (eval "(gensym)")))

(defmessage-handler BOGUS add-em-up around ()
  (+ 1 (call-next-handler)))

(defmessage-handler BOGUS add-em-up before ()
  1)

(defmessage-handler BOGUS add-em-up primary ()
  1)

(defmessage-handler BOGUS add-em-up after ()
  1)

(deffunction testit()
  (make-instance a of THINGO)
  (send [a] print)
  (send [a] quoxnar)
  (send [a] fribban)
  (make-instance b of PHLEGM)
  (send [b] put-x 8)
  (bind ?result-1 (neq (send [b] get-x) 8))
  (send [b] put-y 5)
  (bind ?result-2 (neq (send [b] get-y) around-override))
  (send [b] put-r 2)
  (bind ?result-3 (neq (send [b] get-r) 2))
  (if (or ?result-1 ?result-2 ?result-3)
     then (printout t "Test 4 Failed." crlf)
     else (printout t "Test 4 Passed." crlf))
  (send [b] delete)
  (if (not (instance-existp [a]))
     then 
     (printout t "Test 5 Failed." crlf)
     else
     (send [a] delete)
     (if (or (instance-existp [a]) 
             (any-instancep ((?x THINGO)) TRUE)
             (neq (find-instance ((?x THINGO)) TRUE) (create$)))
        then 
        (printout t "Test 5 Failed." crlf)
        else
        (printout t "Test 5 Passed." crlf)))
  (make-instance [a] of THINGO 
     (q (create$ x1 y1 z1)) 
     (s (create$ a1 b1 c1))
     (r 189.981)
     (y always))

  (if (or (neq (send [a] get-q) (create$ x1 y1 z1))
          (neq (send [a] get-s) (create$ a1 b1 c1))
          (neq (send [a] get-z) (create$ k 1 9.4))
          (neq (send [a] get-r) 189.981)
          (neq (send [a] get-x) s)
          (neq (send [a] get-y) always))
     then (printout t "Test 6 Failed." crlf)
     else (printout t "Test 6 Passed." crlf))
  (initialize-instance [a] 
     (q (create$ q1 s1 r1)) 
     (z (create$ d1 e1 f1))
     (r never)
     (x 345.543))
  (if (or (neq (send [a] get-q) (create$ q1 s1 r1))
          (neq (send [a] get-z) (create$ d1 e1 f1))
          (neq (send [a] get-s) (create$ d e "f"))
          (neq (send [a] get-r) never)
          (neq (send [a] get-x) 345.543)
          (neq (send [a] get-y) 3.5))
     then (printout t "Test 7 Failed." crlf)
     else (printout t "Test 7 Passed." crlf))
   TRUE
  )




