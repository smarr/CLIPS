(defclass A (is-a USER)
  (role concrete)
  (slot foo (type NUMBER))
  (slot bar))

(defclass B (is-a A)
  (slot foo (visibility public)))

(defmessage-handler A woz ()
   ?self:foo)

(defmessage-handler A quoxnar (?value)
   (bind ?self:foo 34 ?value))

(defmessage-handler B fribban ()
   ?self:bar)

(definstances A-B
  (a of A)
  (b of B))

(deffunction go ()
  (reset)
  (send [a] woz)
  (send [b] woz))