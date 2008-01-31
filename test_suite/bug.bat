(deftemplate example (field x (type INTEGER)))
(deftemplate example (field x (type INTEGER)))
(deftemplate example (field x (allowed-integers 1 2 3)))
(deftemplate example (field x (range 1 3)))
(deftemplate example (field x (type INTEGERFLOAT)))
(defgeneric foo)
(deffunction foo ())
(clear)

