(deffunction sign (?num)
  (if (< ?num 0) then
    (return -1))  
  (if (> ?num 0) then
    (return 1))
  0)

(defmethod generic-sign ((?num NUMBER))
  (if (< ?num 0) then
    (return -1))  
  (if (> ?num 0) then
    (return 1))
  0)

(defmessage-handler NUMBER sign ()
  (if (< ?self 0) then
    (return -1))  
  (if (> ?self 0) then
    (return 1))
  0)

(deffunction iterate (?num)
  (bind ?i 0)
  (while TRUE do
    (if (>= ?i ?num) then
       (break))
    (printout t ?i " ")
    (bind ?i (+ ?i 1)))
  (printout t crlf))

(deffunction do-break ()
   (while TRUE do (break)))

(deffunction nested-break ()
   (while 1 do (break)))

(deffunction test-return-in-while ()
   (bind ?i 50)
   (while 1 do
      (return ?i)
      (bind ?i (+ ?i 1))))

