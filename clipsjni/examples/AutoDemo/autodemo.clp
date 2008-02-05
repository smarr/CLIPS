
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
  
  (assert (UI-state (display WelcomeMessage)
                    (relation-asserted start)
                    (state initial)
                    (valid-answers))))

;;;***************
;;;* QUERY RULES *
;;;***************

(defrule determine-engine-state ""

   (logical (start))

   =>

   (assert (UI-state (display StartQuestion)
                     (relation-asserted engine-starts)
                     (response No)
                     (valid-answers No Yes))))
   
(defrule determine-runs-normally ""

   (logical (engine-starts Yes))

   =>

   (assert (UI-state (display RunQuestion)
                     (relation-asserted runs-normally)
                     (response No)
                     (valid-answers No Yes))))

(defrule determine-rotation-state ""

   (logical (engine-starts No))

   =>

   (assert (UI-state (display RotateQuestion)
                     (relation-asserted engine-rotates)
                     (response No)
                     (valid-answers No Yes))))
   
(defrule determine-sluggishness ""

   (logical (runs-normally No))

   =>

   (assert (UI-state (display SluggishQuestion)
                     (relation-asserted engine-sluggish)
                     (response No)
                     (valid-answers No Yes))))
   
(defrule determine-misfiring ""

   (logical (runs-normally No))

   =>

   (assert (UI-state (display MisfireQuestion)
                     (relation-asserted engine-misfires)
                     (response No)
                     (valid-answers No Yes))))

(defrule determine-knocking ""

   (logical (runs-normally No))

   =>

   (assert (UI-state (display KnockQuestion)
                     (relation-asserted engine-knocks)
                     (response No)
                     (valid-answers No Yes))))

(defrule determine-low-output ""

   (logical (runs-normally No))

   =>

   (assert (UI-state (display OutputQuestion)
                     (relation-asserted engine-output-low)
                     (response No)
                     (valid-answers No Yes))))

(defrule determine-gas-level ""

   (logical (engine-starts No)

            (engine-rotates Yes))

   =>

   (assert (UI-state (display GasQuestion)
                     (relation-asserted tank-has-gas)
                     (response No)
                     (valid-answers No Yes))))

(defrule determine-battery-state ""
  
   (logical (engine-rotates No))

   =>
   
   (assert (UI-state (display BatteryQuestion)
                     (relation-asserted battery-has-charge)
                     (response No)
                     (valid-answers No Yes))))

(defrule determine-point-surface-state ""

   (or (logical (engine-starts No)  
   
                (engine-rotates Yes))
                     
       (logical (engine-output-low Yes)))

   =>

   (assert (UI-state (display PointsQuestion)
                     (relation-asserted point-surface-state)
                     (response Normal)
                     (valid-answers Normal Burned Contaminated))))

(defrule determine-conductivity-test ""
   
   (logical (engine-starts No)  
   
            (engine-rotates No)
            
            (battery-has-charge Yes))

   =>

   (assert (UI-state (display CoilQuestion)
                     (relation-asserted conductivity-test-positive)
                     (response No)
                     (valid-answers No Yes))))

;;;****************
;;;* REPAIR RULES *
;;;****************

(defrule normal-engine-state-conclusions ""

   (logical (runs-normally Yes))
   
   =>

   (assert (UI-state (display NoRepair)
                     (state final))))

(defrule engine-sluggish ""

   (logical (engine-sluggish Yes))
   
   =>

   (assert (UI-state (display FuelLineRepair)
                     (state final))))

(defrule engine-misfires ""

   (logical (engine-misfires Yes))

   =>

   (assert (UI-state (display PointGapRepair)
                     (state final))))

(defrule engine-knocks ""

   (logical (engine-knocks Yes))

   =>

   (assert (UI-state (display AdjustTimingRepair)
                     (state final))))

(defrule tank-out-of-gas ""

   (logical (tank-has-gas No))

   =>

   (assert (UI-state (display AddGasRepair)
                     (state final))))
   
(defrule battery-dead ""

   (logical (battery-has-charge No))
   
   =>

   (assert (UI-state (display ReplaceBatteryRepair)
                     (state final))))
   
(defrule point-surface-state-burned ""

   (logical (point-surface-state Burned))

   =>

   (assert (UI-state (display ReplacePointsRepair)
                     (state final))))
                     
(defrule point-surface-state-contaminated ""
   
   (logical (point-surface-state Contaminated))
   
   =>

   (assert (UI-state (display CleanPointsRepair)
                     (state final))))

(defrule conductivity-test-positive-yes ""

   (logical (conductivity-test-positive Yes))
   
   =>

   (assert (UI-state (display LeadWireRepair)
                     (state final))))
                     
(defrule conductivity-test-positive-no ""

   (logical (conductivity-test-positive No))
      
   =>

   (assert (UI-state (display CoilRepair)
                     (state final))))
                     
(defrule no-repairs ""

   (declare (salience -10))
  
   (logical (UI-state (id ?id)))
   
   (state-list (current ?id))
     
   =>
  
   (assert (UI-state (display MechanicRepair)
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
   
