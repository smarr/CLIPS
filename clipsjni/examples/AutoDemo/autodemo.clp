
;;;======================================================
;;;   Automotive Expert System
;;;
;;;     This expert system diagnoses some simple
;;;     problems with a car.
;;;
;;;     CLIPS Version 6.3 Example
;;;
;;;     For use with the Auto Demo Example
;;;======================================================

;;; ***************************
;;; * DEFTEMPLATES & DEFFACTS *
;;; ***************************

(deftemplate UI-state
   (slot id (default-dynamic (gensym*)))
   (slot display)
   (slot relation-asserted (default none))
   (slot response (default none))
   (multislot valid-answers)
   (slot state (default middle)))
   
(deftemplate state-list
   (slot current)
   (multislot sequence))
  
(deffacts startup
   (state-list))
   
;;;****************
;;;* STARTUP RULE *
;;;****************

(defrule system-banner ""

  =>
  
  (assert (UI-state (display "Welcome to the Engine Diagnosis Expert System.")
                    (relation-asserted start)
                    (state initial)
                    (valid-answers))))

;;;***************
;;;* QUERY RULES *
;;;***************

(defrule determine-engine-state ""

   (logical (start))

   =>

   (assert (UI-state (display "Does the engine start?")
                     (relation-asserted engine-starts)
                     (response No)
                     (valid-answers No Yes))))
   
(defrule determine-runs-normally ""

   (logical (engine-starts Yes))

   =>

   (assert (UI-state (display "Does the engine run normally?")
                     (relation-asserted runs-normally)
                     (response No)
                     (valid-answers No Yes))))

(defrule determine-rotation-state ""

   (logical (engine-starts No))

   =>

   (assert (UI-state (display "Does the engine rotate?")
                     (relation-asserted engine-rotates)
                     (response No)
                     (valid-answers No Yes))))
   
(defrule determine-sluggishness ""

   (logical (runs-normally No))

   =>

   (assert (UI-state (display "Is the engine sluggish?")
                     (relation-asserted engine-sluggish)
                     (response No)
                     (valid-answers No Yes))))
   
(defrule determine-misfiring ""

   (logical (runs-normally No))

   =>

   (assert (UI-state (display "Does the engine misfire?")
                     (relation-asserted engine-misfires)
                     (response No)
                     (valid-answers No Yes))))

(defrule determine-knocking ""

   (logical (runs-normally No))

   =>

   (assert (UI-state (display "Does the engine knock?")
                     (relation-asserted engine-knocks)
                     (response No)
                     (valid-answers No Yes))))

(defrule determine-low-output ""

   (logical (runs-normally No))

   =>

   (assert (UI-state (display "Is the output of the engine low?")
                     (relation-asserted engine-output-low)
                     (response No)
                     (valid-answers No Yes))))

(defrule determine-gas-level ""

   (logical (engine-starts No)

            (engine-rotates Yes))

   =>

   (assert (UI-state (display "Does the tank have any gas in it?")
                     (relation-asserted tank-has-gas)
                     (response No)
                     (valid-answers No Yes))))

(defrule determine-battery-state ""
  
   (logical (engine-rotates No))

   =>
   
   (assert (UI-state (display "Is the battery charged?")
                     (relation-asserted battery-has-charge)
                     (response No)
                     (valid-answers No Yes))))

(defrule determine-point-surface-state ""

   (or (logical (engine-starts No)  
   
                (engine-rotates Yes))
                     
       (logical (engine-output-low Yes)))

   =>

   (assert (UI-state (display "What is the surface state of the points?")
                     (relation-asserted point-surface-state)
                     (response Normal)
                     (valid-answers Normal Burned Contaminated))))

(defrule determine-conductivity-test ""
   
   (logical (engine-starts No)  
   
            (engine-rotates No)
            
            (battery-has-charge Yes))

   =>

   (assert (UI-state (display "Is the conductivity test for the ignition coil positive?")
                     (relation-asserted conductivity-test-positive)
                     (response No)
                     (valid-answers No Yes))))

;;;****************
;;;* REPAIR RULES *
;;;****************

(defrule normal-engine-state-conclusions ""

   (logical (runs-normally Yes))
   
   =>

   (assert (UI-state (display "Suggested Repair: None.")
                     (state final))))

(defrule engine-sluggish ""

   (logical (engine-sluggish Yes))
   
   =>

   (assert (UI-state (display "Suggested Repair: Clean the fuel line.")
                     (state final))))

(defrule engine-misfires ""

   (logical (engine-misfires Yes))

   =>

   (assert (UI-state (display "Suggested Repair: Adjust point gap.")
                     (state final))))

(defrule engine-knocks ""

   (logical (engine-knocks Yes))

   =>

   (assert (UI-state (display "Suggested Repair: Adjust timing.")
                     (state final))))

(defrule tank-out-of-gas ""

   (logical (tank-has-gas No))

   =>

   (assert (UI-state (display "Suggested Repair: Add gas.")
                     (state final))))
   
(defrule battery-dead ""

   (logical (battery-has-charge No))
   
   =>

   (assert (UI-state (display "Suggested Repair: Charge the battery.")
                     (state final))))
   
(defrule point-surface-state-burned ""

   (logical (point-surface-state Burned))

   =>

   (assert (UI-state (display "Suggested Repair: Replace the points.")
                     (state final))))
                     
(defrule point-surface-state-contaminated ""
   
   (logical (point-surface-state Contaminated))
   
   =>

   (assert (UI-state (display "Suggested Repair: Clean the points.")
                     (state final))))

(defrule conductivity-test-positive-yes ""

   (logical (conductivity-test-positive Yes))
   
   =>

   (assert (UI-state (display "Suggested Repair: Replace the distributor lead wire.")
                     (state final))))
                     
(defrule conductivity-test-positive-no ""

   (logical (conductivity-test-positive No))
      
   =>

   (assert (UI-state (display "Suggested Repair: Replace the ignition coil.")
                     (state final))))
                     
(defrule no-repairs ""

   (declare (salience -10))
  
   (logical (UI-state (id ?id)))
   
   (state-list (current ?id))
     
   =>
  
   (assert (UI-state (display "Suggested Repair: Take your car to a mechanic.")
                     (state final))))
                     
;;;*************************
;;;* GUI INTERACTION RULES *
;;;*************************

(defrule ask-question

   (declare (salience 5))
   
   (UI-state (id ?id))
   
   ?f <- (state-list (sequence $?s&:(not (member$ ?id ?s))))
             
   =>
   
   (modify ?f (current ?id)
              (sequence ?id ?s))
   
   (halt))

(defrule handle-next-no-change-none-middle-of-chain

   (declare (salience 10))
   
   ?f1 <- (next ?id)

   ?f2 <- (state-list (current ?id) (sequence $? ?nid ?id $?))
                      
   =>
      
   (retract ?f1)
   
   (modify ?f2 (current ?nid))
   
   (halt))

(defrule handle-next-response-none-end-of-chain

   (declare (salience 10))
   
   ?f <- (next ?id)

   (state-list (sequence ?id $?))
   
   (UI-state (id ?id)
             (relation-asserted ?relation))
                   
   =>
      
   (retract ?f)

   (assert (add-response ?id)))   

(defrule handle-next-no-change-middle-of-chain

   (declare (salience 10))
   
   ?f1 <- (next ?id ?response)

   ?f2 <- (state-list (current ?id) (sequence $? ?nid ?id $?))
     
   (UI-state (id ?id) (response ?response))
   
   =>
      
   (retract ?f1)
   
   (modify ?f2 (current ?nid))
   
   (halt))

(defrule handle-next-change-middle-of-chain

   (declare (salience 10))
   
   (next ?id ?response)

   ?f1 <- (state-list (current ?id) (sequence ?nid $?b ?id $?e))
     
   (UI-state (id ?id) (response ~?response))
   
   ?f2 <- (UI-state (id ?nid))
   
   =>
         
   (modify ?f1 (sequence ?b ?id ?e))
   
   (retract ?f2))
   
(defrule handle-next-response-end-of-chain

   (declare (salience 10))
   
   ?f1 <- (next ?id ?response)
   
   (state-list (sequence ?id $?))
   
   ?f2 <- (UI-state (id ?id)
                    (response ?expected)
                    (relation-asserted ?relation))
                
   =>
      
   (retract ?f1)

   (if (neq ?response ?expected)
      then
      (modify ?f2 (response ?response)))
      
   (assert (add-response ?id ?response)))   

(defrule handle-add-response

   (declare (salience 10))
   
   (logical (UI-state (id ?id)
                      (relation-asserted ?relation)))
   
   ?f1 <- (add-response ?id ?response)
                
   =>
      
   (str-assert (str-cat "(" ?relation " " ?response ")"))
   
   (retract ?f1))   

(defrule handle-add-response-none

   (declare (salience 10))
   
   (logical (UI-state (id ?id)
                      (relation-asserted ?relation)))
   
   ?f1 <- (add-response ?id)
                
   =>
      
   (str-assert (str-cat "(" ?relation ")"))
   
   (retract ?f1))   

(defrule handle-prev

   (declare (salience 10))
      
   ?f1 <- (prev ?id)
   
   ?f2 <- (state-list (sequence $?b ?id ?p $?e))
                
   =>
   
   (retract ?f1)
   
   (modify ?f2 (current ?p))
   
   (halt))
   
