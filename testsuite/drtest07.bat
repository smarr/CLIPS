(clear)                            ; DR0602
(subset (mv-append bar)            ; DR0602
        (mv-append "bar"))         ; DR0602 - FALSE
(member fox (mv-append "fox"))     ; DR0602 - FALSE
(clear)                            ; DR0604
(defrule foo => (assert (x)))      ; DR0604
(bsave "Temp//foo.bin")            ; DR0604
(clear)                            ; DR0604
(defrule bar => (assert (y)))      ; DR0604
(bsave "Temp//bar.bin")            ; DR0604
(clear)                            ; DR0604
(bload "Temp//foo.bin")            ; DR0604
(reset)                            ; DR0604
(run)                              ; DR0604
(facts)                            ; DR0604
(bload "Temp//bar.bin")            ; DR0604
(reset)                            ; DR0604
(run)                              ; DR0604
(facts)                            ; DR0604
(clear)                            ; DR0606

(defrule blah                             ; DR0606
   =>                                     ; DR0606
  (bsave "Temp//me.bin")                  ; DR0606
  (bload "Temp//me.bin"))                 ; DR0606
(reset)                            ; DR0606
(run)                              ; DR0606
(tan (deg-rad 270))                ; DR0609 - Error
(sec (deg-rad 270))                ; DR0609 - Error
(clear)                            ; DR0610
(progn (release-mem) TRUE)         ; DR0610
(deffunction foo ($?x) ?x ?x FALSE)
(foo a b c d e)                    ; DR0610
(clear)                            ; DR0610
(progn (release-mem) TRUE)         ; DR0610 - Memory Loss
(clear)                            ; DR0612
(release-mem)                      ; DR0612
(defrule x (not (a)) =>)           ; DR0612
(clear)                            ; DR0612
(progn (release-mem) TRUE)         ; DR0612 - Memory Loss
(clear)                            ; DR0613
(release-mem)                      ; DR0613
(defrule x (not (a)) (not (b))  =>) 
(clear)                            ; DR0613
(progn (release-mem) TRUE)         ; DR0613 - Memory Loss
(clear)                            ; DR0614
(release-mem)                      ; DR0614
(defglobal ?*x* = 3)               ; DR0614
(defglobal ?*x* = 4)               ; DR0614
(clear)                            ; DR0614
(progn (release-mem) TRUE)         ; DR0614 - Memory Loss
(clear)                            ; DR0615
(release-mem)                      ; DR0615
(deftemplate x (multifield y) (multifield y))
(clear)                            ; DR0615
(progn (release-mem) TRUE)         ; DR0615 - Memory Loss
(clear)                            ; DR0616
(release-mem)                      ; DR0616
(any-instancep ((?a OBJECT) (?a OBJECT)) TRUE)
(clear)                            ; DR0616
(progn (release-mem) TRUE)         ; DR0616 - Memory Loss
(clear)                            ; DR0617
(release-mem)                      ; DR0617
(bind ?a 3)                        ; DR0617
(clear)                            ; DR0617
(progn (release-mem) TRUE)         ; DR0617 - Memory Loss
(clear)                            ; DR0619
(str-explode "")                   ; DR0619
(deftemplate foo (field x) (field y))
(str-explode "foo")                ; DR0619
(format t "%s"                     ; DR0621
   "The allowed-values attribute cannot be used in conjunction with
                      other value restriction attributes")
(clear)                            ; DR0623
(defclass A (is-a USER) (role abstract)
   (multislot foo (create-accessor read-write)))
(defclass B (is-a A)               ; DR0623
   (slot foo (propagation no-inherit) (storage shared) (create-accessor read-write)))
(defclass C (is-a B)               ; DR0623
   (slot foo (source composite) (access read-only) (create-accessor read)))
(describe-class C)                 ; DR0623
(clear)                            ; DR0625
(progn (release-mem) TRUE)         ; DR0625
(deftemplate a (field one))        ; DR0625 
(defrule a                         ; DR0625
   ?f1 <- (a (one two three))
   =>)
(clear)                            ; DR0625
(progn (release-mem) TRUE)         ; DR0625 - Memory Loss
(clear)                            ; DR0626
(deffacts one                      ; DR0626
   (first =(assert let's see)))    ; DR0626 - Bad PPForm
(clear)                            ; DR0627
(deftemplate a (field one))        ; DR0627
(defrule a                         ; DR0627
   ?f1 <- (a (one two three))      ; DR0627
   =>                              ; DR0627
   (assert (not good)))            ; DR0627
(clear)                            ; DR0628
(defmethod foo (?a))               ; DR0628
(defmethod foo ((?a NUMBER)))      ; DR0628
(defmethod foo ((?a INTEGER FLOAT))) 
(list-defmethods)                  ; DR0628 - Check Precedence
(clear)                            ; DR0629
(defclass A (is-a USER)            ; DR0629
   (role concrete)
   (multislot foo (default a) (create-accessor read-write)))
(deffunction foo (?ins)            ; DR0629
   (bind ?a (send ?ins get-foo))
   (printout t ?a " ")
   (printout t (send ?ins get-foo) " ")
   (mv-slot-replace ?ins foo 1 1 1)
   (printout t ?a " ")
   (printout t (send ?ins get-foo) crlf))
(make-instance a of A)             ; DR0629
(foo [a])                          ; DR0629 - (a) (a) (a) (1)
(clear)                            ; DR0630
(defrule blah                      ; DR0630
   ?f <- (i-f)
   (test (progn (retract ?f) TRUE))
   =>)
(assert (i-f))                     ; DR0630
(facts)                            ; DR0630
(agenda)                           ; DR0630
(clear)                            ; DR0630
(assert (i-f))                     ; DR0630
(defrule blah                      ; DR0630
   ?f <- (i-f)
   (test (progn (retract ?f) TRUE))
   =>)
(facts)                            ; DR0630
(agenda)                           ; DR0630
(clear)                            ; DR0633
(set-incremental-reset FALSE)      ; DR0633
(defrule not-fire (not (color)) =>)
(agenda)                           ; DR0633
(assert (color))                   ; DR0633
(agenda)                           ; DR0633
(clear)                            ; DR0633
(set-incremental-reset TRUE)       ; DR0633
(clear)                            ; DR0634
(deffunction foo ()                ; DR0634
   (bind ?i 1000)                  ; DR0634
   (while (> ?i 0) do              ; DR0634
      (bind ?a (mv-append))        ; DR0634
      (bind ?i (- ?i 1))))         ; DR0634
(foo)                              ; DR0634
(clear)                            ; DR0635
(format nil "1%N2%3$Q%d3%W" 5)     ; DR0635
(clear)                            ; DR0636
(defrule blah (fact $?x here $?x) =>)
(assert (fact 1 2 here 3 4))       ; DR0636
(agenda)                           ; DR0636
(subsetp (mv-append a a b) (mv-append a b)) ; DR0637
(clear)                            ; DR0639
(defrule foo (or (a) (b) (c)) =>)  ; DR0639
(assert (a) (b) (c))               ; DR0639
(agenda)                           ; DR0639
(run)                              ; DR0639
(refresh foo)                      ; DR0639
(agenda)                           ; DR0639
(clear)                            ; DR0640
(defclass A (is-a USER) (role concrete)); DR0640
(make-instance a of A)             ; DR0640
(defglobal ?*x* = (instance-address a))
(initialize-instance a)            ; DR0640
(clear)                            ; DR0642
(deffunction foo ()
   (do-for-all-instances ((?a USER)) TRUE (send ?a print))
   (printout t "Finished." crlf))
(ppdeffunction foo)                ; DR0642
(clear)                            ; DR0644
(ppdeffunction bad)                ; DR0644
(clear)                            ; DR0645
(deffacts start (result # 0 1 0))
(defrule print-responses
   (result $?input # $?response)
   =>
   (while (neq ?response (create$)) do
      (nth 1 ?response)
      (bind ?response (create$))))
(reset)                            ; DR0645
(run)                              ; DR0645
(clear)                            ; DR0647
(defclass A (is-a USER) (role abstract) (slot x (create-accessor read-write)))  ; DR0647
(defclass B (is-a A))              ; DR0647
(describe-class B)                 ; DR0647
(constructs-to-c bug 1 Temp/)      ; DR0647
(describe-class B)                 ; DR0647
(clear)                            ; DR0648
(bsave "Temp//none.bin")           ; DR0648
(deffunction one ())               ; DR0648
(bsave "Temp//some.bin")           ; DR0648
(bload "Temp//none.bin")           ; DR0648
(clear)                            ; DR0649
(div a)                            ; DR0649
(div 1)                            ; DR0649
(asin 2)                           ; DR0649
(atanh 1.0)                        ; DR0649
(clear)                            ; DR0650
(deffacts foo)                     ; DR0650
(defrule foo =>)                   ; DR0650
(deftemplate foo)                  ; DR0650
(deffacts *)                       ; DR0650
(defrule * =>)                     ; DR0650
(deftemplate *)                    ; DR0650
(list-deffacts)                    ; DR0650
(list-defrules)                    ; DR0650
(list-deftemplates)                ; DR0650
(clear)                            ; DR0654
(fetch "XYZBEDQ.txt")              ; DR0654
(clear)                            ; DR0655
(defclass A (is-a USER) (slot x (create-accessor read-write)))  ; DR0655
(defclass B (is-a A) (slot y (create-accessor read-write)))     ; DR0655
(defmessage-handler B get-y ())    ; DR0655
(clear)                            ; DR0656
(defclass A (is-a USER) (role concrete) (multislot x (create-accessor read-write)))
(make-instance a of A (x (instance-address a) 34))
(save-instances "Temp//ins.sav")   ; DR0656
(clear)                            ; DR0657
(defclass A (is-a USER) (role concrete) (slot x (create-accessor read-write)))
(defmessage-handler A foo (?a) 
   (bind ?x 1)
   (bind ?y 2)
   (put x ?self)) 
(make-instance a of A)             ; DR0657
(send [a] foo 1)                   ; DR0657
(clear)                            ; DR0658
(defclass A (is-a USER) (role concrete) (multislot x (create-accessor read-write)))
(make-instance a of A)             ; DR0658
(mv-slot-insert [a] x 1 abc)       ; DR0658
(clear)                            ; DR0659
(defclass a (is-a USER) ())        ; DR0659
(defclass b (is-a USER) (slot))    ; DR0659
(list-defclasses)                  ; DR0659
(clear)                            ; DR0660
(definstances bad (a of))          ; DR0660
(clear)                            ; DR0661
(definstances bad (a of BOGUS))    ; DR0661
(reset)                            ; DR0661
(facts)                            ; DR0661
(clear)                            ; DR0662
(sub-string 0 0 abc)               ; DR0662
(clear)                            ; DR0663
(defmessage-handler USER * ())     ; DR0663
(list-defmessage-handlers USER)    ; DR0663 - changed
(clear)                            ; DR0664
(defglobal ?*x* = 0)               ; DR0664
(defclass A (is-a USER)            ; DR0664
   (slot x (default ?*x*) (create-accessor read-write))
   (slot y (default-dynamic ?*x*) (create-accessor read-write)))
(clear)                            ; DR0665
(preview-send OBJECT bogus)        ; DR0665
(clear)                            ; DR0666
(defglobal ?*x* = 300)             ; DR0666
(defclass A (is-a USER) (role concrete)); DR0666
(make-instance a of A)             ; DR0666
(while (> ?*x* 0) do 
   (any-instancep ((?a A)) TRUE)
   (bind ?*x* (- ?*x* 1)))
(do-for-all-instances ((?a A)) TRUE (printout t "Foo!" crlf))
(clear)                            ; DR0668
(defclass a (is-a USER) (role abstract))
(defclass b (is-a USER) (role abstract))
(defclass c (is-a a b))            ; DR0668
(defclass d (is-a a))              ; DR0668
(defclass e (is-a c d))            ; DR0668
(clear)                            ; DR0668
(defclass a (is-a USER) (role abstract))
(defclass b (is-a USER) (role abstract))
(defclass c (is-a a))              ; DR0668
(defclass d (is-a b))              ; DR0668
(defclass e (is-a c d))            ; DR0668
(defclass f (is-a e a b))          ; DR0668
(describe-class f)                 ; DR0668
(clear)                            ; DR0670
(defrule foo => (close blah))      ; DR0670
(reset)                            ; DR0670
(run)                              ; DR0670
(clear)                            ; DR0672
(defglobal ?*x* = (create$ a b c d))
(deffunction foo ()
   (bind ?y ?*x*)
   (printout t ?y " " ?*x* crlf)
   (bind ?*x* (create$ e f g h))
   (printout t ?y " " ?*x* crlf))
(foo)                              ; DR0672
(clear)
(reset)                   ; DR0675
(defrule foo (initial-fact) (test (> 4 3)) =>)
(agenda)
(assert (a))
(defrule bar (initial-fact) (or (test (> 5 3)) (a)) =>)
(agenda)
(reset)
(assert (a))
(agenda)
(clear)
(defmethod foo (?a)       ; DR0676
  (bind ?b 10)
  (call-next-method)
  ?b)
(defmethod foo ($?any)
  (bind ?b 20))
(foo bar)
(clear)
(call-next-method 0)      ; DR0677
(next-methodp 0)
(preview-generic)
(clear)
(delete-instance 0)       ; DR0678
(init-slots 0)
(ppinstance 0)
(clear)
(print-region 1 2 3)      ; DR0679
(print-region t 2 3)
(clear)
(defclass A (is-a USER))  ; DR0680
(class-superclasses A inherit)
(defclass A (is-a USER))
(class-superclasses A inherit)
(clear)
;; DRs 0681, 0682 and 0683 involve recompiling CLIPS
(while 1 do ?a)           ; DR0684
(clear)
(printout t ?a crlf)      ; DR0685
(defrule foo =>
   (printout t ?a)
   (bind ?a 1))
(reset)
(run)
(clear)
(str-cat abc (eval "(+ abc)") def) ; DR0686
(sym-cat abc (eval "(+ abc)") def)
(clear)
(deffunction foo (?a))    ; DR0687
(foo (eval "(+ abc)"))
(clear)
(defgeneric any-instancep) ; DR0688
(defgeneric assert)
(defgeneric bind)
(defgeneric break)
(defgeneric call-next-handler)
(defgeneric call-next-method)
(defgeneric delayed-do-for-all-instances)
(defgeneric do-for-all-instances)
(defgeneric do-for-instance)
(defgeneric duplicate)
(defgeneric expand$)
(defgeneric find-all-instances)
(defgeneric find-instance)
(defgeneric if)
(defgeneric initialize-instance)
(defgeneric make-instance)
(defgeneric modify)
(defgeneric next-handlerp)
(defgeneric next-methodp)
(defgeneric override-next-handler)
(defgeneric progn)
(defgeneric retract)
(defgeneric return)
(defgeneric while)
(clear)
(defmethod blah  ;; DR0689
  ((?a (progn (undefmethod blah *) TRUE))))
(blah 1)
(clear)
(defgeneric foo)  ;; DR0690
(foo (progn (undefgeneric foo) 1))
(deffunction bar (?a))
(bar (progn (undeffunction bar) 1))
(clear)
(printout t (eval "(+ abc)") " OOPS " (eval "(+ abc)") crlf)  ;; DR0691
(clear)
(deftemplate foo             ;; DR0692
  (multifield linkTagList))
(defrule foo
   ?w<-(foo)
   =>
   (modify ?w (linkTagList ?linktag ?linktagx ?a $?b)))
(clear)
(unwatch all)  ;; DR0693
(defrule foo (logical (not (a))) => (assert (b)))
(watch facts)
(reset)
(run)
(assert (a))
(reset)
(defrule bar (declare (salience -1)) => (assert (a)))
(run)
(unwatch all)
(clear)
(defclass A (is-a USER) (role concrete)
  (slot foo (create-accessor read-write))
  (multislot bar (create-accessor read-write))) ;; DR0697 and DR0700
(make-instance fribban of A)
(make-instance quoxnar of A)
(send [quoxnar] put-foo (instance-address [fribban]))
(send [quoxnar] put-bar (mv-append))
(unmake-instance [fribban])
(send (send [quoxnar] get-foo) print)
(save-instances "Temp//badfile.tmp")
(reset)
(restore-instances "Temp//badfile.tmp")
(send [quoxnar] print)
(clear)
(defglobal ?*x* = 0) ;; DR0698 and DR0701
(deffunction create-number-2 ()
  (bind ?*x* (nth 1 (str-explode "2.0")))
  BOGUS)
(deffunction create-number ()
  (create-number-2))
(deffunction force-garbage-collection ()
  (bind ?i 0)      
  (while (< ?i 10000) do
    (gensym*)
    (bind ?i (+ ?i 1))))
(mv-append (create-number) ?*x* (bind ?*x* 0)
  (force-garbage-collection) (float (random))
     (float (random)))
(clear)
(deffunction foo ()      ;; DR0699
   (printout t Foo crlf))
(defmethod bar () (foo))
(bar)
(undeffunction *)
(bar)
(ppdeffunction foo)
(clear) ;; DR0700
(defclass A (is-a USER) (role concrete) (multislot foo (create-accessor read-write)))
(make-instance a of A (foo (mv-append)))
(send [a] print)
(save-instances "Temp//ins.tmp")
(unmake-instance [a])
(load-instances "Temp//ins.tmp")
(send [a] print)
(clear) ;; DR0704
(defrule foo => (printout t [hdh] crlf))
(reset)
(run)
(bsave "Temp//foo.bin")
(clear)
(bload "Temp//foo.bin")
(clear)
