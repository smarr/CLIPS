
(defclass BEclass (is-a USER)
  (role concrete)
  (slot sName (create-accessor read-write))
  (multislot supertypes (create-accessor read-write))
  (multislot subtypes (create-accessor read-write))
  (multislot properties (create-accessor read-write))
  (slot separateCompilation (create-accessor read-write))
  (slot parent (create-accessor read-write))
  (multislot Vproperties (create-accessor read-write))
  (multislot values (create-accessor read-write))
  (slot controlCaption (create-accessor read-write))
  (slot	windowName (create-accessor read-write))
  (slot	defaultInstance	(create-accessor read-write))
  (multislot viewProperty (create-accessor read-write))
  (slot	controlType (create-accessor read-write))
  (slot	index (create-accessor read-write))
  (slot	x (create-accessor read-write))
  (slot	width (create-accessor read-write))
  (slot	height (create-accessor read-write)))

(defclass BEwindow 
  (is-a USER)			
  (role concrete)	
  (slot sName (create-accessor read-write))
  (slot windowCaption (create-accessor read-write)))

(defclass BECPMheader 
  (is-a USER)
  (role concrete)	
  (slot sName (create-accessor read-write)))

(deffunction BEeditInit (?IOportName)
  (loop-for-count (?i 1 20) do
    (make-instance of BEwindow))
  (make-instance of BEclass (sName RootClass))
  (make-instance of BECPMheader)
  (return TRUE))

(deffunction BECPMIn(?gFN)
  (reset)
  (load-instances ?gFN))
