(clear)                            ; DR0501
(defmessage-handler USER foo ())   ; DR0501
(clear)                            ; DR0501
(list-defmessage-handlers)         ; DR0501 - None
(defclass a (is-a USER))           ; DR0501
(defmessage-handler a foo ())      ; DR0501
(defclass a (is-a USER))           ; DR0501
(list-defmessage-handlers a)       ; DR0501 - Update 1/1/93
(conserve-mem off)                 ; DR0501
(defmessage-handler USER foo ())   ; DR0501
(conserve-mem on)                  ; DR0501
(defmessage-handler USER foo ())   ; DR0501
(clear)                            ; DR0501
(progn (release-mem) TRUE)         ; DR0501
(conserve-mem off)                 ; DR0501
(clear)                            ; DR0502
(defclass a (is-a USER))           ; DR0502
(defmessage-handler a get-bar ())  ; DR0502
(defclass a (is-a USER) (slot bar (create-accessor ?NONE))) ;; Update 1/1/93
(list-defmessage-handlers a)       ; DR0502 - None
(clear)                            ; DR0503
(defglobal ?*global-list* = (mv-append 1 2 3 4))
(defrule foo                       ; DR0503
   (items $?list)
   (test (subset ?list ?*global-list*))
   =>
   (printout t "Success!" crlf))
(assert (items 2 3))               ; DR0503
(run)                              ; DR0503 - Success!
(clear)                            ; DR0504
(defrule blah =>)                  ; DR0504
(deffacts a (x y) (z q))           ; DR0504
(deftemplate foob (field x))       ; DR0504
(defglobal ?*x* = 6)               ; DR0504
(deffunction quox (?x) (* ?x 3))   ; DR0504
(defclass hip (is-a USER))         ; DR0504
(bsave "Temp//drtest06.bin")       ; DR0504
(bload "Temp//drtest06.bin")       ; DR0504
(bload "Temp//drtest06.bin")       ; DR0504
(clear)                            ; DR0504
(clear)                            ; DR0506
(retract *)                        ; DR0506
(clear)                            ; DR0508
(deffacts list-fact                ; DR0508
   (list 12 "=" 3.0 i2))
(defrule test-member               ; DR0508
   (list $?list)
   =>
   (printout t "position=" (member i2 ?list) crlf))
(reset)                            ; DR0508
(run)                              ; DR0508 - position=4
(clear)                            ; DR0511
(deftemplate calculate             ; DR0511
   (field operation
      (type SYMBOL) (default ?NONE)))
(assert (calculate (operation +))) ; DR0511
(modify 1 
   (calculate (operation op2)))    ; DR0511 - Error
(duplicate 1 
   (calculate (operation word)))   ; DR0511 - Error
(clear)                            ; DR0512
(deftemplate calculate             ; DR0512
   (field operation
      (type SYMBOL) (default ?NONE)))
(assert (calculate 
        (operation 4<56)))         ; DR0512 - Error
(assert (calculate (operation go)))
(modify 1 (operation 467<789)))    ; DR0512 - Error
(duplicate 1 (operation 54<2345))) ; DR0512 - Error
(clear)                            ; DR0517
(deftemplate r                     ; DR0517
   (field mine) (field yours))     ; DR0517
(watch facts)                      ; DR0517
(assert (r (mine "string")))       ; DR0517
(assert (r (yours this-is-a-word)))
(modify 1 (mine "string"))         ; DR0517
(modify 2 (yours wordie))          ; DR0517
(modify 4 (yours is-mine))         ; DR0517
(unwatch facts)                    ; DR0517
(clear)                            ; DR0519
(deftemplate result                ; DR0519
   (field x
      (type NUMBER)
      (allowed-floats 5.0  6.0  7.0  8.8  9.0  8.0)
      (allowed-integers  1 2 3 4)
      (default ?NONE)))
(assert (result (x 6)))            ; DR0519
(clear)                            ; DR0520
(assert (a) (b) (c))               ; DR0520
(facts)                            ; DR0520
(facts 1 3 2)                      ; DR0520
(facts 3)                          ; DR0520
(not FALSE)                        ; DR0521 - TRUE
(not "FALSE")                      ; DR0521 - FALSE
(clear)                            ; DR0522
(deftemplate foo (field x))        ; DR0522
(assert (foo (x (1 2))))           ; DR0522 - Error
(facts)                            ; DR0522 - None
(print-region 1 1)                 ; DR0524 - Error
(print-region 3 "foo.lis" ROOT SUBTOPIC)    
(print-region t 3 ROOT SUBTOPIC)   ; DR0524 - Error
(deffunction mftest ()             ; DR0525
   (bind ?result (mv-append))
   (bind ?i 140)
   (while (> ?i 0) do
      (bind ?result (mv-append ?i ?result))
      (bind ?i (- ?i 1)))
   ?result)
(mftest)                           ; DR0525
(clear)                            ; DR0526
(defrule dr0384                    ; DR0526
   =>
   (bind ?a (** 2 (- -11 (numberp 3))))
   (if (and (< ?a 0.000245)  (>=  ?a 0.000244))
       then (assert (DR0384 OK))))
(seed (5))                         ; DR0527
(sym-cat (str-explode "a s d g e f")) ; DR0528 - Error
(str-length                        ; DR0529 - Error
   (str-implode ((mv-append 1 2 3 4)
						           (mv-append 4 3 2 1))))
(deg-grad 90)                      ; DR0532 - 100.0
(* 3.6 15.0)                       ; DR0533 - 54.0
(* 3.0 15.0)                       ; DR0533 - 45.0
898~898                            ; DR0536 - 898
(format nil "%d" 12)               ; DR0539 - "12"
(format nil "|%d|" 12)             ; DR0539 - "|12|"
(clear)                            ; DR0540
(defglobal ?*x* = ?*r*)            ; DR0540 - Error
(defglobal ?*w* = 4)               ; DR0540 - OK
(clear)                            ; DR0541
(defclass a (is-a OBJECT) (role abstract))
(defclass b (is-a OBJECT) (role abstract))
(defclass c (is-a a))              ; DR0541
(defclass d (is-a b))              ; DR0541
(defclass e (is-a c d))            ; DR0541
(defclass f (is-a e b a))          ; DR0541
(describe-class e)                 ; DR0541 - e c a d b OBJECT
(describe-class f)                 ; DR0541 - f e c d b a OBJECT
(clear)                            ; DR0542
(defclass a (is-a USER)            ; DR0542
   (slot x (default (+ 3 (eval "(gensym)")))))
(clear)                            ; DR0543
(deffunction blah () (if a b))     ; DR0543 - Error
(blah)                             ; DR0543 - Undefined
(clear)                            ; DR0548
(set-incremental-reset FALSE)      ; DR0548
(defrule foo (not (a)) =>)         ; DR0548
(reset)                            ; DR0548
(agenda)                           ; DR0548 - 1 Activation
(clear)                            ; DR0548
(set-incremental-reset TRUE)       ; DR0548
(clear)                            ; DR0549
(defclass A (is-a USER) (role concrete) (slot xy (create-accessor read-write)))
(make-instance a of A 
   ((sym-cat x y) 34))             ; DR0549
(send [a] print)                   ; DR0549
(clear)                            ; DR0551
(set-strategy breadth)             ; DR0551
(defrule blah (declare (salience 10)) =>)
(defrule flub (declare (salience -10)) =>)
(reset)                            ; DR0551
(agenda)                           ; DR0551 - blah, flub
(set-strategy depth)               ; DR0551
(clear)                            ; DR0552
(defclass A (is-a USER) (role concrete)); DR0552
(defclass B (is-a USER) (role concrete)); DR0552
(make-instance a1 of A)            ; DR0552 - [a1]
(make-instance a2 of A)            ; DR0552 - [a2]
(make-instance b1 of B)            ; DR0552 - [b1]
(make-instance b2 of B)            ; DR0552 - [b2]
(do-for-all-instances ((?a A)) TRUE
   (do-for-all-instances ((?b B)) TRUE
     (printout t (instance-name ?a) " " 
                 (instance-name ?b) crlf))) ; [a1][b1], [a1][b2], [a2][b1], [a2][b2]
(format nil "%d1234567890123456789012345678901234567890" 333) ; DR0559
(clear)                            ; DR0561
(deftemplate foo                   ; DR0561
   (field x (type EXTERNAL-ADDRESS)))
(assert (foo))                     ; DR0561
(facts)                            ; DR0561
(clear)                            ; DR0562
(deffunction foo ()                ; DR0562
   (+ ?a 1) (bind ?a 2))           ; DR0562
(foo)                              ; DR0562 - Error
(clear)                            ; DR0566
(bind ?a 3)                        ; DR0566 - 3
(bind ?b 4)                        ; DR0566 - 4
(+ ?a 3)
(+ ?a ?b)
(reset)
(+ ?a 3)                           ; DR0566 - Error
(+ ?a ?b)                          ; DR0566 - Error
(+ ?e 4)                           ; DR0566 - Error
(clear)                            ; DR0567
(deffunction foo (?a ?a))          ; DR0567 - Error
(format t "%s (Yes or No)" "Play Again?") ; DR0568
(clear)                            ; DR0569
(defclass A (is-a USER)            ; DR0569
   (role concrete)
   (slot unassigned-parcels)
   (slot x-location (create-accessor read-write)))
(make-instance a of A)             ; DR0569 - [a]
(send [a] put-x-location 34)       ; DR0569 - TRUE
(clear)                            ; DR0570
(deffunction defgeneric ())        ; DR0570 - Error
(deffunction defclass ())          ; DR0570 - Error
(deffunction deftemplate ())       ; DR0570 - Error
(deffunction defmethod ())         ; DR0570 - Error
(deffunction deffacts ())          ; DR0570 - Error
(list-deffunctions)                ; DR0570 - None
(clear)                            ; DR0571
(defglobal ?*x* = 3)               ; DR0571
(assert (a))                       ; DR0571
(defrule foo ?f <- (a) => (bind ?*x* ?f))
(run)                              ; DR0571
(printout t ?*x* crlf)             ; DR0571
?*x*                               ; DR0571
(clear)                            ; DR0573
(defclass A (is-a USER))           ; DR0573
(defclass B (is-a USER))           ; DR0573
(defclass C (is-a A B) (role concrete)) ; DR0573
(make-instance c of C)             ; DR0573 - [c]
(instances)                        ; DR0573 - 1 instance
(instances MAIN B inherit)              ; DR0573 - 1 instance
(do-for-all-instances ((?u USER)) TRUE
    (printout t ?u crlf))          ; DR0573 - 1 instance
(clear)                            ; DR0575
(defrule try-or (or (a) (b)) =>)   ; DR0575
(bsave "Temp//drtest06.bin")       ; DR0575
(clear)                            ; DR0575
(bload "Temp//drtest06.bin")       ; DR0575
(clear)                            ; DR0575
(clear)                            ; DR0576
(deffunction bar () (eval "(+ 5 6)"))
(deffunction foo () (eval "(+ 3 4 (bar))"))
(foo)                              ; DR0576 - 18
(bar)                              ; DR0576 - 11
(clear)                            ; DR0577
(defclass A (is-a USER) (slot foo (create-accessor read-write)))
(defclass B (is-a A))              ; DR0577
(defmessage-handler A put-foo ())  ; DR0577 - Error
(defmessage-handler B get-foo ())  ; DR0577 - OK
(defmessage-handler B put-foo ())  ; DR0577 - OK
(clear)                            ; DR0584 - Behavior changed
(defclass A (is-a USER)            ; DR0584
   (role concrete)
   (slot foo (create-accessor ?NONE) (access read-only) (default 5)))
(defmessage-handler A put-foo primary (?value)
  (dynamic-put foo ?value))
(make-instance a of A)             ; DR0584 - [a]
(instances)                        ; DR0584
(save-instances "Temp//drtest06.tmp")    ; DR0584
(unmake-instance a)                ; DR0584
(load-instances "Temp//drtest06.tmp")    ; DR0584
(instances)                        ; DR0584
(restore-instances "Temp//drtest06.tmp") ; See CRS
(instances)                        ; DR0584
(clear)                            ; DR0588
(ppdefinstances Teams)             ; DR0588
(clear)                            ; DR0590
(undefinstances *)                 ; DR0590
(undefinstances Tom)               ; DR0590
(clear)                            ; DR0591
(defrule two                       ; DR0591
   =>
  (clear)
  (printout t "Rule Fired" crlf))
(reset)                            ; DR0591    
(run)                              ; DR0591
(clear)                            ; DR0592
(deftemplate color                 ; DR0592
   (field standard
      (type SYMBOL)
      (default white)
      (allowed-symbols red white blue)))
 (defrule one                      ; DR0592 - Error
    ?f1 <- (color (standard ?))
    =>
    (modify ?f1 (standard none)))
(progn (release-mem) TRUE)         ; DR0592
(clear)                            ; DR0594
(defrule junk => (bsave "Temp//drtest06.bin"))     
(reset)                            ; DR0594
(run)                              ; DR0594
(bload "Temp//drtest06.bin")       ; DR0594
(reset)                            ; DR0594
(run)                              ; DR0594
(div 11.0 .5)                      ; DR0595
(clear)                            ; DR0600
(defmethod blah 1 ())              ; DR0600
(defmethod blah (?a))              ; DR0600
(list-defmethods)                  ; DR0600
