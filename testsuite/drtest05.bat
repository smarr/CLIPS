(clear)                            ; DR0405
(deffacts first (fact1 =(reset)))  ; DR0405
(reset)                            ; DR0405
(facts)                            ; DR0405
(clear)                            ; DR0410
(defrule foo                       ; DR0410
   (fact ?x&:(and (< ?x 3) (> ?x 1)))
   =>)
(assert (fact a))                  ; DR0410 - Only one error message
(clear)                            ; DR0411
(defrule foo                       ; DR0411
   (fact ?x)
   (fact $?y)
   (test (eq ?y (str-explode ?x)))
   =>)
(reset)                            ; DR0411
(assert (fact "a b c"))            ; DR0411
(assert (fact a b c))              ; DR0411
(agenda)                           ; DR0411
(facts)                            ; DR0411
(save-facts "Temp//foo.tmp")       ; DR0411
(reset)                            ; DR0411
(load-facts "Temp//foo.tmp")       ; DR0411
(facts)                            ; DR0411
(agenda)                           ; DR0411
(clear)                            ; DR0427
(deftemplate first (fild one))     ; DR0427
(list-deftemplates)                ; DR0427
(eval (str-cat                     ; DR0435
      (printout t " fact-list "))) ; DR0435
(clear)                            ; DR0441
(deftemplate a)                    ; DR0441
(assert (a))                       ; DR0441
(clear)                            ; DR0441
(list-deftemplates)                ; DR0441
(clear)                            ; DR0445
(deftemplate congestion            ; DR0445
   (field no-of-nets))
(deftemplate total                 ; DR0445
   (field net-name)
   (field cong))
(deffacts start                    ; DR0445
   (congestion (no-of-nets 5))
   (total (net-name 8) (cong nil))
   (total (net-name 4) (cong 5)))
(defrule p403                      ; DR0445
   ?t1 <-  (total (cong nil))
   (congestion (no-of-nets ?non))
   =>
   (retract ?t1))
(defrule p410                      ; DR0445
   (total (net-name ?nn) (cong ?non))
   (not (total (cong nil)))
   ?t <- (total (net-name ~?nn) (cong ?x&:(<= ?x ?non)))
   =>
   (retract ?t))
(watch facts)                      ; DR0445
(reset)                            ; DR0445
(run)                              ; DR0445
(unwatch facts)                    ; DR0445
(clear)                            ; DR0447
(deffacts one                      ; DR0447
   (first =(assert let's see)))    ; DR0447
(clear)                            ; DR0451
(defrule erroneous-syntax-error    ; DR0451
   (fact1 test ?symbol&:(eq ?symbol :) ?num)
   =>) 
(clear)                            ; DR0452
(deftemplate a (fileld one) (field two))
(defrule b                         ; DR0452
   (not (a (one first) (three second)))
   => 
   (assert (problem)))             ; DR0452
(clear)                            ; DR0453
(deftemplate a (field one))        ; DR0453
(defrule a                         ; DR0453
   ?f1 <- (a (one two three))
   =>
   (assert (not good)))            ; DR0453
(clear)                            ; DR0460
(deftemplate a                     ; DR0460
   (field one) (field two))
(defrule one                       ; DR0460
   ?fact <- (a)
   =>
   (modify ?a (two)))
(clear)                            ; DR0462
(defrule a => (assert (x ?h)))     ; DR0462
(reset)                            ; DR0462
(watch rules)                      ; DR0462
(run)                              ; DR0462
(unwatch rules)                    ; DR0462
(clear)                            ; DR0466
(defrule a 
   ?f1 <- (fact 1)
   =>
   (eval "(retract ?f1)"))
(reset)                            ; DR0466
(assert (fact 1))                  ; DR0466
(run)                              ; DR0466
(facts)                            ; DR0466
(mod 160 100)                      ; DR0475
(mod (+ 100 60) 100)               ; DR0475
(send diamond get-hardness)        ; DR0478
(clear)                            ; DR0479
(defclass FOO (is-a USER)          ; DR0479
   (role concrete)
   (multislot BAR (create-accessor read-write)))
(make-instance foo of FOO)         ; DR0479 - [foo]
(send [foo] put-BAR a b c)         ; DR0479 - TRUE
(find-instance ((?x FOO))          ; DR0479
    (member d (send ?x get-BAR)))  ; DR0479 - ()
(clear)                            ; DR0480
(defclass mineral (is-a USER)      ; DR0480
   (role concrete)
   (slot gemname (create-accessor read-write))
   (slot group (create-accessor read-write)))
(definstances gems                 ; DR0480
   (almandite of mineral
      (gemname almandite) (class garnet))
   (grossularite of mineral
      (gemname grossularite) (group garnet))
   (spessartite of mineral
      (gemname spessartite) (group garnet)))
(reset)                            ; DR0480 - one error message
(clear)                            ; DR0484
(send [bogus-instance] bogus-message) ; DR0484
(clear)                            ; DR0486
(defrule allie                     ; DR0486
  (menu compress)
  (compress ?nos ?range ?no&:(eq ?no 1))
=>
  (printout t "rule allie ?nos= " ?nos " ?range= " ?range " " ?no crlf))
(reset)                            ; DR0486
(assert (menu compress))           ; DR0486
(assert (compress "19k" "date" 1)) ; DR0486
(run)                              ; DR0486
"1234567890
 1234567890
 1234567890
 1234567890
 1234567890
 1234567890
 1234567890
 1234567890
 1234567890
 1234567890"                       ; DR0488
(instance-address [bogus])         ; DR0491 - Error should contain function name
(instance-namep [foo])             ; DR0492 - TRUE
(clear)                            ; DR0494
(defmethod foo (?a))               ; DR0494
(defmethod foo ((?a NUMBER)))      ; DR0494
(preview-generic foo 45)           ; DR0494
(clear)                            ; DR0495
(defmethod foo ())                 ; DR0495
(ppdefmethod foo 1)                ; DR0495
(clear)                            ; DR0496
(defmethod foo ((?a INTEGER SYMBOL)))
(defmethod foo ((?a INTEGER)))     ; DR0496
(list-defmethods)                  ; DR0496
(clear)                            ; DR0499
(defmethod foo 1 (?a))             ; DR0499
(defmethod foo 2 () (undefmethod foo 1))
(foo)                              ; DR0499
(clear)                            ; DR0500
(defclass a (is-a USER) (role concrete))           ; DR0500
(defmessage-handler a create-new-handler ()
   (build "(defmessage-handler a new-handler ())"))
(make-instance a of a)             ; DR0500 - [a]
(send [a] create-new-handler)      ; DR0500 - Error
