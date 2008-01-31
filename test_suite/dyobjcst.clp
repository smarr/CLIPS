;;;************************************************************
;;; DEFCLASS SLOT DYNAMIC ERROR CHECKING
;;;
;;; This file tests a number of common errors which can be
;;; made with defclas slots. Among the errors tested are
;;;   (1) type/range attribute conflicts
;;;   (2) range/allowed-... attribute conflicts
;;;   (3) type/allowed-... attribute conflicts
;;;   (4) type/default attribute conflicts
;;;   (5) range/default attribute conflicts
;;;   (6) allowed-.../default attribute conflicts
;;;   (7) illegal attribute values
;;;
;;;************************************************************

(defclass BASE (is-a USER)
   (role concrete)
   (pattern-match non-reactive)
   (slot any (type ?VARIABLE) (create-accessor write))
   (slot symbol (type SYMBOL) (create-accessor write))
   (slot string (type STRING) (create-accessor write))
   (slot lexeme (type LEXEME) (create-accessor write))
   (slot integer (type INTEGER) (create-accessor write))
   (slot float (type FLOAT) (create-accessor write))
   (slot number (type NUMBER) (create-accessor write))
   (slot instance-name (type INSTANCE-NAME) (create-accessor write))
   (slot instance-address (type INSTANCE-ADDRESS) (create-accessor write))
   (slot instance (type INSTANCE) (create-accessor write))
   (slot external-address (type EXTERNAL-ADDRESS) (create-accessor write))
   (slot fact-address (type FACT-ADDRESS) (create-accessor write))
   (slot address (type EXTERNAL-ADDRESS INSTANCE-ADDRESS FACT-ADDRESS)
                  (create-accessor write))
)

(defclass DERIVE-1-1 (is-a BASE)
  (slot any (source composite) (allowed-values abc 123 "def" 5.0))
  (multislot address (source composite) (cardinality 2 3))
)

(defclass DERIVE-1-2 (is-a BASE)
  (multislot any (source composite)
                 (cardinality 3 5))
  (slot float (source composite) (allowed-floats 3.0 9.0 27.0))
)

(defclass ERROR-1 (is-a BASE)
  (slot float (source composite) (default 36)))

(defclass ERROR-2 (is-a BASE)
  (slot float (source composite) (default-dynamic (sym-cat abc)))
)

(defclass ERROR-3 (is-a DERIVE-1-2)
  (slot float (source composite) (default 36.0))
)

(defclass ERROR-4 (is-a DERIVE-1-2)
  (slot any (source composite) (default))
)

(definstances constraint-tests
  (base of BASE)
  (derive-1-1 of DERIVE-1-1)
  (derive-1-2 of DERIVE-1-2)
)
