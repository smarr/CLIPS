(clear) ;; DR0705
(= 5 (read))
Whatever
(clear) ;; DR0707
(defclass A (is-a USER) (role concrete) (slot foo (create-accessor read-write)))
(make-instance a of A (foo "\"This is a string\""))
(send [a] print)
(save-instances "Temp//bogus.tmp")
(reset)
(instances)
(load-instances "Temp//bogus.tmp")
(send [a] print)
(clear) ;; DR0713

(defclass TEST
  (is-a USER)
  (role concrete)
  (slot item (create-accessor write)))
(make-instance a of TEST)
(message-modify-instance [a] (item 3))
(clear)                   ; DR0724
[x]
(clear)                   ; DRO725

(defrule should-be-ok
   (message $?first)
   (test (length$ ?first))
   (translation $?first)
   =>)
(clear)                   ; DR0726

(defrule bar
   (not (and (c ?x) (d ?y&:(> ?y 3))))
   =>)
(reset)
(assert (c 1) (d a)) ; should not hang
(clear)                   ; DR0727

(defrule foo
   (not (and (a) (b) (c)))
   (d ?x&:(> ?x 3))
   =>)
(reset)
(assert (d a)) ; Error should be for pattern 5
(clear)                   ; DR0728

(deftemplate attempt
   (multifield numbers (default 7 7 3 3))
   (multifield rpn))

(deffacts initial-info
   (attempt)
   (operator *)
   (operator /)
   (operator -)
   (operator +))

(defrule do-first
   ?f <- (attempt (numbers $?b ?n1 $?m ?n2 $?e)
                  (rpn))
   (operator ?o)
   =>
   (duplicate ?f (numbers ?b ?m ?e)
                 (rpn ?n1 ?n2 ?o)))

(defrule do-next
   ?f <- (attempt (numbers $?b ?n $?e)
                  (rpn ?f $?rest))
   (operator ?o)
   =>
   (duplicate ?f (numbers ?b ?e)
                 (rpn ?f ?rest ?n ?o)))
(reset)
(run) ; should not hang or crash
(clear)                   ; DR0729
(get-salience-evaluation)
(set-salience-evaluation every-cycle) ; should return when-defined
(set-salience-evaluation when-defined) ; should return every-cycle
(clear)                   ; DR0730
(get-salience-evaluation)
(defmodule A)
(refresh-agenda *)
(get-salience-evaluation) ; should be when-defined
(clear)                   ; DR0731
(unwatch all)
(defrule zoiks =>)
(reset)
(watch rules)
(defrule zoiks =>) ; activation should not be printed
(unwatch all)
(clear)                   ; DR0734
(defmodule MAIN (export ?ALL))
(defglobal MAIN ?*proximity* = 9)
(defmodule SCORE (import MAIN ?ALL))

(defrule SCORE::should-be-ok
   (attempt1)
   (test (<= 3 ?*proximity*))
   =>)
(clear)                  ; DR0736

(deftemplate where
   (multislot x (type SYMBOL)))

(defrule yak ; This should be OK
   (where (x $?pds&:(member$ x ?pds)))
   =>)
(clear)

(defrule foo ; This should fail
   (bbb ?x&:(member a ?x))
   =>)
(clear)

(deftemplate this
   (slot x)
   (slot y (type INTEGER))
   (multislot z (type STRING)))

(defrule this-1 ; This should fail
   (this (x ?x))
   =>
   (member$ a ?x))
(clear)                   ; DR0737

(defrule fd-1 ; This should be ok
   (a)
   (not (and (b)
             (or (c)
                 (d))))
   =>)
   
(defrule fd-2 ; this should be ok
   (a)
   (exists (b)
           (or (and (c))
               (d)))
   =>)

(defrule fd-3 ; this should be ok
   (a)
   (not (and (b)
             (or (and (c) (e))
                 (d))))
   =>)

(defrule fd-4 ; this should be ok
   (a)
   (exists (b)
           (or (c)         
               (d)))
   =>)
(clear)

(defrule foo-1
   (not (or (a) (b)))
   =>)

(defrule foo-2
   (not (not (or (a) (b))))
   =>)

(defrule foo-3
   (not (and (not (a))
             (not (b))))
   =>)
(reset)
(agenda)
(assert (a))
(agenda)
(assert (b))
(agenda)
(clear)                   ; DR0738

(deffacts f1
   (s 1)
   (s 2)
   (s 3)
   (c)
   (a 1))

(defrule r1 ; SHOULD activate, but doesn't
   (s 1)
   (exists (or (and (a ?x)
                    (d)
                    (test (> 16 10)))
               (and (a ?x)
                    (c))))
   =>)
   
(defrule r2 ; SHOULD activate, but doesn't
   (s 2)
   (not (and (not (and (a ?x)
                       (d)
                       (test (> 16 10))))
              (not (and (a ?x)
                        (c)))))
   =>)

(defrule r3 ; SHOULD NOT activate, but does
   (s 3)
   (not (and (a ?x)
             (d)
             (test (> 16 10))))
   (not (and (a ?x)
             (c)))
   =>)
(reset)
(agenda)
(clear)                    ; DR0740

(deffunction bug (?list ?a ?b)
   (while TRUE (return (subseq$ ?list ?a ?b))))
(bug (create$ a b c) 1 2) ; Should return (a b)

(deffunction bug (?list ?a ?b)
   (loop-for-count 1 (return (subseq$ ?list ?a ?b))))
(bug (create$ a b c) 1 2) ; Should return (a b)
(clear)                   ; DR0742 - should have no error
(deftemplate TREATMENT
   (multislot values
   (type INTEGER)
   (range 0 19)
   (cardinality 0 20)))
(assert (TREATMENT (values (create$))))
(clear)                  ; DR0743 - should have no error
(defglobal ?*x* = 3)
(bsave "Temp//mytest.bin")
(clear)
(watch all)
(bload "Temp//mytest.bin")
(unwatch all)
(clear)                 ; DR0745
(unwatch all)
(defclass A (is-a USER)
  (role concrete)
  (pattern-match reactive)
  (slot x (create-accessor write))
  (slot y (create-accessor write)))
(defrule bug
  (object (is-a A)
          (y 100)
          (x 100))
  =>)
(agenda) ; should be no activations
(make-instance a of A)
(watch activations)
(object-pattern-match-delay ; should have only one activation arrow
   (send [a] put-y 100)
   (send [a] put-x 100))
(unwatch all)
(clear)                 ; DR0747

(defclass A (is-a USER)
   (slot foo (type INSTANCE-NAME)))
(slot-types A foo) ; Should be just instance-name
(clear)                ; DR0748 - Should have no error from print message
(defmodule MAIN (export ?ALL))
(defclass TEST (is-a USER)
  (role concrete))
(defmodule TEST (import MAIN ?ALL))
(make-instance test of TEST)
(do-for-instance ((?t TEST)) TRUE
  (send ?t print))
(clear)                ; DR0749
(defclass A (is-a USER)
   (slot foo (access read-only) (create-accessor ?NONE)))
(slot-facets A foo) ; facet should be SHR and not LCL
(clear)                ; DR0750 - Should not crash
(defclass FOO (is-a USER) (slot x))
(slot-cardinality FOO x)
(clear)                ; DR0752 - Should return "a"
(deffunction y ()
   (progn$ (?x (create$ "a" "b" "c"))
      (return ?x))
   (return "hello"))
(y)
(clear)                ; DR0753

(defmodule MAIN (export ?ALL))

(defrule MAIN::foo
  =>
  (assert (a))
  (focus B)
  (return))

(defrule MAIN::bar
  (a)
  =>
  (focus B)
  (return))

(defmodule B (import MAIN ?ALL))

(defrule B::yak
  =>)
(reset)
(run 1)
(list-focus-stack) ; Just B should be on stack
(reset)
(focus MAIN B)
(list-focus-stack) ; Should be MAIN B MAIN
(run 1)
(list-focus-stack) ; Should be B B MAIN
(clear)                ; DR0754

(defclass A (is-a USER)
  (role concrete)
  (pattern-match reactive)
  (slot foo (create-accessor read-write)))

(defclass B (is-a A)
  (pattern-match non-reactive))

(defrule foo
  (i-f)
  (test (send [b] put-foo blah))
  =>)
(make-instance b of B)
(assert (i-f)) ; Should not generate error
(clear)                ; DR0756 - Should not crash
(defclass A (is-a USER) (role concrete))
(make-instance a of A)
(any-instancep ((?long-var A)) ?long-var:)
(clear)                ; DR0757 - Error ID should be [INSMNGR11]
(defclass A (is-a USER) (role concrete))
(make-instance BOGUS::A of A)
(clear)                ; DR0758

(defclass A (is-a USER)
  (role concrete))

(defglobal ?*x* = (assert (foo))
           ?*y* = (make-instance [a] of A))
(reset)
(facts)
(instances)
(clear)                ; DR0759

(deftemplate A
   (slot foo (type INTEGER)
             (allowed-instance-names [a])))
(clear)                ; DR0763

(deffunction bug (?state)
   (switch ?state
     (case instance then TRUE)
     (case logical then TRUE)
     (default (return error))))
(bsave "Temp//foo.bin")
(clear)                ; DR0765

(deftemplate bar
   (slot foo (type INTEGER)
             (allowed-values abc def)))
(clear)                ; DR0766

(deftemplate temp
   (multislot foofoo (type INTEGER LEXEME))
   (slot after-foo (type STRING)))
(set-dynamic-constraint-checking TRUE)
(assert (temp (foofoo 11)))

(deftemplate temp2
   (multislot foofoo (type INTEGER LEXEME)))
(assert (temp2 (foofoo (progn 3.4))))
(set-dynamic-constraint-checking FALSE)
(clear)                ; DR0770

(defclass A (is-a USER)
  (role concrete)
  (pattern-match reactive)
  (multislot foo (default x)))

(defrule bomb
  (object (foo $? x $?))
  (object (foo $? ? $?))
=>)
(make-instance a of A)
(clear)                ; DR0778

(deffacts example
   (a b c d e d c b a a a a a a a a a a a a))

(defrule foo
   (a ?x $?y d $?z ?x)
   =>
   (printout t "x = " ?x " y = " ?y " z = " ?z crlf))
(reset)
(agenda)
(clear)               ; DR0782
(set-strategy breadth)
(set-strategy depth)
(deftemplate t1       ; DR0783
  (field code)
  (field a))
(deftemplate t2
  (field code)
  (field a))
(deftemplate t1
  (field code)
  (field a))
(deftemplate t2
  (field code)
  (field b))
(clear)

(defclass fifty (is-a USER)     
  (role   concrete)       
  (pattern-match  reactive)
  (slot   s1      (type   SYMBOL))
  (slot   s2      (type   INTEGER))
  (slot   s3      (type   SYMBOL))
  (slot   s4      (type   INTEGER))
  (slot   s5      (type   SYMBOL))
  (slot   s6      (type   INTEGER))
  (slot   s7      (type   SYMBOL))
  (slot   s8      (type   INTEGER))
  (slot   s9      (type   SYMBOL))
  (slot   s10     (type   INTEGER))
  (slot   s11     (type   SYMBOL))
  (slot   s12     (type   INTEGER))
  (slot   s13     (type   SYMBOL))
  (slot   s14     (type   INTEGER))
  (slot   s15     (type   SYMBOL))
  (slot   s16     (type   INTEGER))
  (slot   s17     (type   SYMBOL))
  (slot   s18     (type   INTEGER))
  (slot   s19     (type   SYMBOL))
  (slot   s20     (type   INTEGER))
  (slot   s21     (type   SYMBOL))
  (slot   s22     (type   INTEGER))
  (slot   s23     (type   SYMBOL))
  (slot   s24     (type   INTEGER))
  (slot   s25     (type   SYMBOL))
  (slot   s26     (type   INTEGER))
  (slot   s27     (type   SYMBOL))
  (slot   s28     (type   INTEGER))
  (slot   s29     (type   SYMBOL))
  (slot   s30     (type   INTEGER))
  (slot   s31     (type   SYMBOL))
  (slot   s32     (type   INTEGER))
  (slot   s33     (type   SYMBOL))
  (slot   s34     (type   INTEGER))
  (slot   s35     (type   SYMBOL))
  (slot   s36     (type   INTEGER))
  (slot   s37     (type   SYMBOL))
  (slot   s38     (type   INTEGER))
  (slot   s39     (type   SYMBOL))
  (slot   s40     (type   INTEGER))
  (slot   s41     (type   SYMBOL))
  (slot   s42     (type   INTEGER))
  (slot   s43     (type   SYMBOL))
  (slot   s44     (type   INTEGER))
  (slot   s45     (type   SYMBOL))
  (slot   s46     (type   INTEGER))
  (slot   s47     (type   SYMBOL))
  (slot   s48     (type   INTEGER))
  (slot   s49     (type   SYMBOL))
  (slot   s50     (type   INTEGER)))

(deffunction f(?limit ?class)
   (reset)
   (loop-for-count (?i 1 ?limit)
      (bind ?t (time))
      (loop-for-count (?j 1 500) (make-instance of ?class))
      (printout t ?i crlf)))
(f 3 fifty)
(clear)               ; DR0784

(defrule testing
   ?f1<-(orders $?first&:(> (length$ ?first) 0))
   ?f2<-(orders $?others&:(subsetp ?first ?others))
   =>)

(defrule testing
   (orders $?first&:(implode$ ?first)
                   :(implode$ ?first))
   =>)
(clear)               ; DR0785
(defglobal ?*x* = 0)
(defmodule MAIN (export ?ALL))
(defmodule FOO (import MAIN ?ALL))
(defclass FOO (is-a USER) (role concrete))
(bind ?*x* (instance-address (make-instance foo of FOO)))
(focus MAIN)
(send [FOO::foo] print)
(send ?*x* print)
(clear)               ; DR0786

(deftemplate nanu
   (slot a (type INTEGER))
   (slot b (type FLOAT)))
(assert (nanu))
(modify 1 (c 4))
(clear)               ; DR0788

(defrule r1 
   (or (P1 p1) (P2 p2)) 
   =>)
(reset)
(bsave "Temp//r1.bin")
(clear)
(bload "Temp//r1.bin")
(reset)
(watch all)
(unwatch all)
(clear)               ; DR0789
(deffacts MAIN:: the-suit-list)
(clear)               ; DR0790

(defrule init
   =>
  (assert (p 1)))

(defrule crash
  (p ?X)
  (not (test (eq ?X 1)))
  (p ?Y)
  (not (and (test (neq ?Y 20))(test (neq ?Y 30))))
  =>)
(reset)
(run)
(clear)               ; DR0791

(defrule autorule2_B2
  (or (foo ?st_B1)
      (bpc-newdata))
  =>
  ?st_B1)
(clear)                   ; DR0793
(load bug793.clp)
(BEeditInit streamout)
(BECPMIn bug793.ins)
(clear)                   ; DR0795

(defrule rule-1
  (blah $?y)
  =>
  (progn$ (?x ?y) (printout t ?x)))
(ppdefrule rule-1)
(clear)                   ; DR0798
(reset)
(assert (a))
(assert (b))
(defrule t1 (b) (a) =>)
(defrule t2 (a) (or (b) (b)) =>)
(agenda)
(clear)
