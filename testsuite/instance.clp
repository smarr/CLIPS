(defclass A (is-a USER)
  (role concrete)
  (slot x (create-accessor read-write) (default 34))
  (slot y (create-accessor write) (default abc)))
(defmessage-handler A put-x before (?value)
  (printout t "Slot x set with message." crlf))
(defmessage-handler A delete after ()
  (printout t "Old instance deleted." crlf))

(defclass BOGUS (is-a USER)
  (role concrete)
  (slot a  (create-accessor write)
           (default-dynamic 
             (progn (printout t "Slot a set by class." crlf) abc)))
  (slot b  (create-accessor write)
           (default-dynamic 
             (progn (printout t "Slot b set by class." crlf) def))))

(defmessage-handler BOGUS delete before ()
  (printout t "Deleting old copy of instance." crlf))

(defmessage-handler BOGUS init before ()
  (printout t "New instance received init message." crlf))

(defmessage-handler BOGUS put-a before (?val)
  (printout t "Slot a in instance set with put- message." crlf))

(defmessage-handler BOGUS put-b before (?val)
  (printout t "Slot b in instance set with put- message." crlf))

(defclass B (is-a USER)
  (role concrete)
  (slot x (create-accessor read-write) (default 34))
  (slot y (create-accessor write) (default 100))
  (slot z (create-accessor write) (default ?NONE)))

(defclass C (is-a USER)
  (role concrete)
  (slot x  (create-accessor read-write) (default 1)))
(definstances C-OBJECTS
  (c1 of C)
  ((gensym) of C (x 65)))

(defclass D (is-a USER)
  (role concrete)
  (slot x  (create-accessor read-write) (default abc)))

(defclass W (is-a USER) (role concrete))
(defclass X (is-a W))
(defclass Y (is-a W))
(defclass Z (is-a X Y))

(defclass SAVE-TEST (is-a USER) (role concrete)
   (slot stamp (access read-only)  (create-accessor read) 
   (default-dynamic (time))))
