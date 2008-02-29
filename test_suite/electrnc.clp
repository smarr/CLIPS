;;;======================================================
;;;   Circuit Input/Output Simplification Expert System
;;;
;;;     This program simplifies the boolean decision 
;;;     table for a circuit consisting of inputs (SOURCES) 
;;;     and outputs (LEDs). 
;;;
;;;     The simplification procedure works as follows:
;;;     1) The connections between components of the
;;;        circuit are initialized.
;;;     2) The response of the circuit when all SOURCEs
;;;        are set to zero is determined.
;;;     3) Source input values are changed one at a time
;;;        and the response of the circuit is determined.
;;;        All possible input combinations are iterated
;;;        through using a gray code (a number representation
;;;        system using binary digits in which successive
;;;        integers differ by exactly one binary digit).
;;;        For example, the gray code for the numbers 0 to 7
;;;        is 0 = 000, 1 = 001, 2 = 011, 3 = 010, 4 = 110,
;;;        5 = 111, 6 = 101, 7 = 100. By using a gray code,
;;;        only one SOURCE has to be changed at a time to
;;;        determine the next response in the decision 
;;;        table (minimizing execution time).
;;;     4) As responses are determined, a rule checks to
;;;        see if any two sets of inputs with the same
;;;        response differ if a single input. If so, then
;;;        the single input can be replaced with a * 
;;;        (indicating that it does not matter what the
;;;        value of the input is given the other inputs).
;;;        For example,  if the input 0 1 0 gave a response
;;;        of 1 0 and the input 0 0 0 gave the same response,
;;;        then the decision table can be simplified by
;;;        indicating that 0 * 0 gives a response of 1 0.
;;;     5) Once all responses and simplifications have been
;;;        determined, the decision table for the circuit is
;;;        printed.
;;;        
;;;     This example illustrates the use of most of the
;;;     constructs available in CLIPS 6.0 and also shows how
;;;     COOL can be effectively integrated with rules.
;;;     Generic functions are used to connect the components
;;;     of the circuit during initialization. Classes,
;;;     message-handlers, and deffunctions are used to
;;;     determine the response of the circuit to a set of
;;;     inputs. Rules, deffunctions, and global variables
;;;     are used to control execution, iterate through all
;;;     possible input combinations, simplify the boolean
;;;     decision tree, and print out the simplified decision
;;;     tree.
;;;
;;;     CLIPS Version 6.0 Example
;;; 
;;;     To execute, load this file, load one of the circuit
;;;     files (circuit1.clp, circuit2.clp, or circuit3.clp), 
;;;     reset, and run.
;;;======================================================


;;;***********
;;; DEFCLASSES
;;;***********

(defclass COMPONENT
  (is-a USER)
  (slot ID# (create-accessor write)))

(defclass NO-OUTPUT
  (is-a USER)
  (slot number-of-outputs (access read-only) 
                          (default 0)
                          (create-accessor read)))

(defmessage-handler NO-OUTPUT compute-output ())

(defclass ONE-OUTPUT
  (is-a NO-OUTPUT)
  (slot number-of-outputs (access read-only) 
                          (default 1)
                          (create-accessor read))
  (slot output-1 (default UNDEFINED) (create-accessor write))
  (slot output-1-link (default GROUND) (create-accessor write))
  (slot output-1-link-pin (default 1) (create-accessor write)))

(defmessage-handler ONE-OUTPUT put-output-1 after (?value)
   (send ?self:output-1-link 
         (sym-cat put-input- ?self:output-1-link-pin)
         ?value))

(defclass TWO-OUTPUT
  (is-a ONE-OUTPUT)
  (slot number-of-outputs (access read-only) 
                          (default 2)
                          (create-accessor read))
  (slot output-2 (default UNDEFINED) (create-accessor write))
  (slot output-2-link (default GROUND) (create-accessor write))
  (slot output-2-link-pin (default 1) (create-accessor write)))

(defmessage-handler TWO-OUTPUT put-output-1 after (?value)
   (send ?self:output-2-link 
         (sym-cat put-input- ?self:output-2-link-pin)
         ?value))

(defclass NO-INPUT
  (is-a USER)
  (slot number-of-inputs (access read-only) 
                         (default 0)
                         (create-accessor read)))

(defclass ONE-INPUT
  (is-a NO-INPUT)
  (slot number-of-inputs (access read-only) 
                         (default 1)
                         (create-accessor read))
  (slot input-1 (default UNDEFINED) 
                (visibility public)
                (create-accessor read-write))
  (slot input-1-link (default GROUND) (create-accessor write))
  (slot input-1-link-pin (default 1) (create-accessor write)))

(defmessage-handler ONE-INPUT put-input-1 after (?value)
   (send ?self compute-output))

(defclass TWO-INPUT
  (is-a ONE-INPUT)
  (slot number-of-inputs (access read-only) 
                         (default 2)
                         (create-accessor read))
  (slot input-2 (default UNDEFINED) 
                (visibility public)
                (create-accessor write))
  (slot input-2-link (default GROUND) (create-accessor write))
  (slot input-2-link-pin (default 1) (create-accessor write)))

(defmessage-handler TWO-INPUT put-input-2 after (?value)
   (send ?self compute-output))
 
(defclass SOURCE
  (is-a NO-INPUT ONE-OUTPUT COMPONENT)
  (role concrete)
  (slot output-1 (default UNDEFINED) (create-accessor write)))

(defclass SINK
  (is-a ONE-INPUT NO-OUTPUT COMPONENT)
  (role concrete)
  (slot input-1 (default UNDEFINED) (create-accessor read-write)))

;;;*******************
;;; NOT GATE COMPONENT
;;;*******************

(defclass NOT-GATE
  (is-a ONE-INPUT ONE-OUTPUT COMPONENT)
  (role concrete))

(deffunction not# (?x) (- 1 ?x))

(defmessage-handler NOT-GATE compute-output ()
   (if (integerp ?self:input-1) then
       (send ?self put-output-1 (not# ?self:input-1))))

;;;*******************
;;; AND GATE COMPONENT
;;;*******************

(defclass AND-GATE
  (is-a TWO-INPUT ONE-OUTPUT COMPONENT)
  (role concrete))

(deffunction and# (?x ?y) 
  (if (and (<> ?x 0) (<> ?y 0)) then 1 else 0))

(defmessage-handler AND-GATE compute-output ()
   (if (and (integerp ?self:input-1) 
            (integerp ?self:input-2)) then
       (send ?self put-output-1 (and# ?self:input-1 ?self:input-2))))

;;;******************
;;; OR GATE COMPONENT
;;;******************

(defclass OR-GATE
  (is-a TWO-INPUT ONE-OUTPUT COMPONENT)
  (role concrete))

(deffunction or# (?x ?y) 
  (if (or (<> ?x 0) (<> ?y 0)) then 1 else 0))

(defmessage-handler OR-GATE compute-output ()
   (if (and (integerp ?self:input-1) 
            (integerp ?self:input-2)) then
       (send ?self put-output-1 (or# ?self:input-1 ?self:input-2))))

;;;********************
;;; NAND GATE COMPONENT
;;;********************

(defclass NAND-GATE
  (is-a TWO-INPUT ONE-OUTPUT COMPONENT)
  (role concrete))

(deffunction nand# (?x ?y) 
  (if (not (and (<> ?x 0) (<> ?y 0))) then 1 else 0))

(defmessage-handler NAND-GATE compute-output ()
   (if (and (integerp ?self:input-1) 
            (integerp ?self:input-2)) then
       (send ?self put-output-1 (nand# ?self:input-1 ?self:input-2))))

;;;*******************
;;; XOR GATE COMPONENT
;;;*******************

(defclass XOR-GATE
  (is-a TWO-INPUT ONE-OUTPUT COMPONENT)
  (role concrete))

(deffunction xor# (?x ?y) 
  (if (or (and (= ?x 1) (= ?y 0))
          (and (= ?x 0) (= ?y 1))) then 1 else 0))

(defmessage-handler XOR-GATE compute-output ()
   (if (and (integerp ?self:input-1) 
            (integerp ?self:input-2)) then
       (send ?self put-output-1 (xor# ?self:input-1 ?self:input-2))))

;;;*******************
;;; SPLITTER COMPONENT
;;;*******************

(defclass SPLITTER
  (is-a ONE-INPUT TWO-OUTPUT COMPONENT)
  (role concrete))

(defmessage-handler SPLITTER compute-output ()
   (if (integerp ?self:input-1) then
       (send ?self put-output-1 ?self:input-1)
       (send ?self put-output-2 ?self:input-1)))

;;;**************
;;; LED COMPONENT
;;;**************

(defclass LED
  (is-a ONE-INPUT NO-OUTPUT COMPONENT)
  (role concrete))

;;; Returns the current value of each LED 
;;; instance in a multifield value.
(deffunction LED-response ()
   (bind ?response (create$))
   (do-for-all-instances ((?led LED)) TRUE
      (bind ?response (create$ ?response (send ?led get-input-1))))
   ?response)

;;;***************************
;;; DEFGENERICS AND DEFMETHODS
;;;***************************

(defgeneric connect)

;;; Connects a one output component to a one input component.
(defmethod connect ((?out ONE-OUTPUT) (?in ONE-INPUT)) 
   (send ?out put-output-1-link ?in) 
   (send ?out put-output-1-link-pin 1)
   (send ?in  put-input-1-link ?out)
   (send ?in  put-input-1-link-pin 1))

;;; Connects a one output component to one pin of a two input component.
(defmethod connect ((?out ONE-OUTPUT) (?in TWO-INPUT) (?in-pin INTEGER)) 
   (send ?out put-output-1-link ?in)
   (send ?out put-output-1-link-pin ?in-pin)
   (send ?in  (sym-cat put-input- ?in-pin -link) ?out)
   (send ?in  (sym-cat put-input- ?in-pin -link-pin) 1))

;;; Connects one pin of a two output component to a one input component.
(defmethod connect ((?out TWO-OUTPUT) (?out-pin INTEGER) (?in ONE-INPUT)) 
   (send ?out (sym-cat put-output- ?out-pin -link) ?in)
   (send ?out (sym-cat put-output- ?out-pin -link-pin) 1)
   (send ?in put-input-1-link ?out)
   (send ?in put-input-1-link-pin ?out-pin))

;;; Connects one pin of a two output component 
;;; to one pin of a two input component.
(defmethod connect ((?out TWO-OUTPUT) (?out-pin INTEGER)
                    (?in TWO-INPUT) (?in-pin INTEGER)) 
   (send ?out (sym-cat put-output- ?out-pin -link) ?in)
   (send ?out (sym-cat put-output- ?out-pin -link-pin) ?in-pin)
   (send ?in  (sym-cat put-input- ?in-pin -link) ?out)
   (send ?in  (sym-cat put-input- ?in-pin -link-pin) ?out-pin))

;;;****************************
;;; DEFGLOBALS AND DEFFUNCTIONS 
;;;****************************

(defglobal ?*gray-code* = (create$)
           ?*sources* = (create$)
           ?*max-iterations* = 0)

;;; Given the current iteration, determines the next 
;;; bit in the gray code to change. 
;;; Algorithm courtesy of John R. Kennedy (The BitMan).
(deffunction change-which-bit (?x)
   (bind ?i 1)
   (while (and (evenp ?x) (<> ?x 0)) do 
      (bind ?x (div ?x 2))
      (bind ?i (+ ?i 1)))
   ?i)

;;; Forward declaration since the initial configuration
;;; is stored in a separate file.
(deffunction connect-circuit ())

;;;*********
;;; DEFRULES
;;;*********

(defrule startup
  =>
  ;; Initialize the circuit by connecting the components
  (connect-circuit) 
  ;; Setup the globals. 
  (bind ?*sources* (find-all-instances ((?x SOURCE)) TRUE))
  (do-for-all-instances ((?x SOURCE)) TRUE
     (bind ?*gray-code* (create$ ?*gray-code* 0)))
  (bind ?*max-iterations* (round (** 2 (length ?*sources*))))
  ;; Do the first response.
  (assert (current-iteration 0)))

(defrule compute-response-1st-time
   ?f <- (current-iteration 0)
   =>
   ;; Set all of the sources to zero.
   (do-for-all-instances ((?source SOURCE)) TRUE (send ?source put-output-1 0))
   ;; Determine the initial LED response.
   (assert (result ?*gray-code* =(str-implode (LED-response))))
   ;; Begin the iteration process of looping through the gray code combinations.
   (retract ?f)
   (assert (current-iteration 1)))

(defrule compute-response-other-times
   ?f <- (current-iteration ?n&~0&:(< ?n ?*max-iterations*))
   =>
   ;; Change the gray code, saving the changed bit value.
   (bind ?pos (change-which-bit ?n))
   (bind ?nv (- 1 (nth ?pos ?*gray-code*)))
   (bind ?*gray-code* (replace$ ?*gray-code* ?pos ?pos ?nv))
   ;; Change the single changed source
   (send (nth ?pos ?*sources*) put-output-1 ?nv)   
   ;; Determine the LED response to the input.
   (assert (result ?*gray-code* =(str-implode (LED-response))))
   ;; Assert the new iteration fact
   (retract ?f)
   (assert (current-iteration =(+ ?n 1))))

(defrule merge-responses
   (declare (salience 10))
   ?f1 <- (result $?b  ?x $?e ?response)
   ?f2 <- (result $?b ~?x $?e ?response)
   =>
   (retract ?f1 ?f2)
   (assert (result $?b * $?e ?response)))

(defrule print-header
   (declare (salience -10))
   =>
   (assert (print-results))
   (do-for-all-instances ((?x SOURCE)) TRUE (format t " %3s " (sym-cat ?x)))
   (printout t " | ")
   (do-for-all-instances ((?x LED)) TRUE (format t " %3s " (sym-cat ?x)))
   (format t "%n")
   (do-for-all-instances ((?x SOURCE)) TRUE (printout t "-----"))
   (printout t "-+-")
   (do-for-all-instances ((?x LED)) TRUE (printout t "-----"))
   (format t "%n"))
      
(defrule print-result
   (print-results)
   ?f <- (result $?input ?response)
   (not (result $?input-2 ?response-2&:(< (str-compare ?response-2 ?response) 0)))
   =>
   (retract ?f)
   ;; Print the input from the sources.
   (while (neq ?input (create$)) do
      (printout t "  " (nth 1 ?input) "  ")
      (bind ?input (rest$ ?input)))
   ;; Print the output from the LEDs.
   (printout t " | ")
   (bind ?response (str-explode ?response))
   (while (neq ?response (create$)) do
      (printout t "  " (nth 1 ?response) "  ")
      (bind ?response (rest$ ?response)))
   (printout t crlf))
