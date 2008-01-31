(clear)                   ; DR0801
(setgen 1)
(unwatch all)
(watch instances)
(watch activations)
(watch rules)

(defclass A (is-a USER)
  (role concrete)
  (pattern-match reactive)
  (slot match (default yes) (create-accessor read-write))
  (slot container (create-accessor read-write)))
  
(defmessage-handler A delete before ()
  (if (instance-existp ?self:container) then
     (unmake-instance ?self:container)))
     
(defrule A-rule
  (logical ?obj <- (object (is-a A) (match yes)))
=>
  (send ?obj put-container 
      (make-instance of A (match no)
                          (container (make-instance of INITIAL-OBJECT))))
  (send ?obj put-match no))
(make-instance a of A)
(run)
(unwatch all)
(clear)                   ; DR0802

(defclass A (is-a USER)
  (role concrete)
  (slot foo (default bar)))
  
(defmessage-handler A delete after ()
  (printout t ?self:foo crlf))
(unmake-instance (make-instance of A))
(clear)                   ; DR0803

(defclass A
   (is-a USER)
  (role concrete)
  (pattern-match reactive)
  (multislot data
    (create-accessor read-write)))
    
(defrule rule1
  (object (is-a A) (data 0 ?x))
  (object (is-a A) (data 1 ?x))
  =>
  (printout t ?x crlf))

(definstances objects
  (a of A (data 0 0))
  (b of A (data 1 0))
  (c of A (data 1 1)))
(reset)
(agenda)
(clear)                   ; DR0804

(deffunction imfi (?cv)
   (bind ?position 3)
   (while TRUE do
     (bind ?nv (+ (nth$ ?position ?cv) 1))
     (if (<= ?nv 9)
        then 
        (return (replace$ ?cv ?position ?position ?nv)))
     (bind ?cv (replace$ ?cv ?position ?position 1))
     (bind ?position (- ?position 1))
     (if (< ?position 1) then (return FALSE))))
 
(deffunction optimize ()
   (bind ?current-settings (create$ 1 1 1))
   (while (neq ?current-settings FALSE)
      (bind ?current-settings (imfi ?current-settings))))
(reset)
(optimize)
(clear)                   ; DR0805
(setgen 1)

(defclass A
   (is-a USER)
   (role concrete)
   (pattern-match reactive)
   (multislot data
      (create-accessor read-write)))

(defrule rule1
   (object (is-a A) (data ? red ?x&green))
   (object (is-a A) (data ? red ?x))
   =>)
(make-instance of A (data orange red green))
(matches rule1)
(clear)                   ; DR0806
(setgen 1)

(defclass A (is-a INITIAL-OBJECT)
   (multislot foo))

(defclass B (is-a A)
   (slot foo))

(defrule AB
   (object (is-a A) (foo ?val))
   =>
   (printout t ?val crlf))
(make-instance of B)
(run)
(clear)                   ; DR0807
(insert$ (rest$ (create$ abc def)) 2 ghi)
(clear)                   ; DR0808
(assert (m))
(assert (a))
(defrule r1 (m) (not (a)) =>)
(defrule r2 (m) (not (a)) (not (b)) =>)
(agenda)
(clear)                   ; DR0809
(deffunction pins () (ppinstance))
(defmessage-handler USER pins () (pins))
(defclass A (is-a USER) (role concrete))
(make-instance a of A)
(send [a] pins)
(clear)                   ; DR0810
(deffunction MAIN::foo
   (?garbage)
   (setgen 1)
   (loop-for-count ?garbage
      (make-instance of INITIAL-OBJECT))
   (delayed-do-for-all-instances ((?ins INITIAL-OBJECT))
      TRUE
      (progn
         (unmake-instance *)
         (return (gensym*)))))
(foo 100)
(foo 500)
(clear)                   ; DR0813

(defclass A (is-a INITIAL-OBJECT)
   (multislot foo (create-accessor read-write)))

(defrule A
   (fact ?v)
   (not (object (is-a A) (foo $? ?v $?)))
=>)
(assert (fact a))
(make-instance a of A (foo a b c))
(make-instance b of A (foo a b c))

(object-pattern-match-delay
   (modify-instance a (foo q))
   (modify-instance b (foo q)))
(clear)                   ; DR0815

(defclass grammy (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot  text
    (create-accessor write)
    (type SYMBOL)))

(defmessage-handler grammy print before ()
    (printout t crlf)
    (printout t "******  starting to print   ****"  ?self crlf))

(defmessage-handler grammy print after ()
    (printout t "******  starting to print   ****"  ?self crlf)
    (printout t crlf))

(deffunction resize (?xlist)
   (if (= (length$ ?xlist)  0)
     then
     (printout t "got to here !!! "  crlf)
     (return)
     else
     (make-instance (gensym) of grammy
         (text (subseq$ ?xlist 1 9))))
     (resize (subseq$ ?xlist 10 (length$ ?xlist))))

(deffunction ask ()
   (do-for-all-instances ((?tag grammy)) (instancep ?tag)
      (send ?tag print)))

(defrule commence  "make it happen"
   =>
   (resize (create$ a b c d e f g h i j k l m n)))
(reset)
(run)
(clear)                   ; DR0816

(defclass A
	  (is-a USER)
	  (role concrete)
	  (slot str
	    (create-accessor read-write)
	    (type STRING))
	  (slot length
	    (create-accessor read-write)
	    (type INTEGER)))

(defmessage-handler A put-str after (?value)
   (bind ?self:length 3))
(make-instance a of A (str 4))
(send [a] get-length)
(clear)                   ; DR0817

(deftemplate status 
   (slot search-depth)
   (slot parent))

(defrule move-alone 
  ?node <- (status)
  =>
  (duplicate ?node (search-depth =(+ 1 3))
                   (parent ?node)))
(ppdefrule move-alone) 
 
(deftemplate dbdata
  (multislot values))
 
(defrule bug1
  =>
  (assert (dbdata (values (create$ 1 2)))))
(ppdefrule bug1)
 
(defrule bug2
  =>
  (assert (dbdata (values (create$ 1 2) (create$ 3 4)))))
(ppdefrule bug2)    
 
(deftemplate foo 
   (field x) 
   (multifield y))
 
(deffacts d5 (foo (y a)))
(ppdeffacts d5)
(deffacts d6 (foo (y a b)) (b) (foo (x 3)) (d))
(ppdeffacts d6)
(clear)                   ; DR0818
(defmodule A (export ?ALL))
(defgeneric A::foo)
(defmethod A::foo ((?arg NUMBER)))
(defmodule B (import A ?ALL))
(defclass B (is-a USER))
(defmethod B::foo ((?arg B)))
(clear)                   ; DR0819

(defclass A 
   (is-a INITIAL-OBJECT)
   (multislot foo (create-accessor read-write)))
(make-instance a of A)
(modify-instance [a] (foo 4))
(send [a] print)
(clear)                   ; DR0820

(defclass A (is-a USER)
   (role concrete)
   (slot iii 
      (type INTEGER)
      (default -1)
      (visibility public)
      (create-accessor read-write)))

(defclass B (is-a USER)
   (role concrete)
   (slot ooo 
      (type INSTANCE)
      (visibility public)
      (create-accessor read-write)))

(defmessage-handler B init after ()
   (send [a] put-iii 23)
   (printout t "1st output line: iii = " (send [a] get-iii) crlf)
   (initialize-instance [a])
   (printout t "2nd output line: iii = " (send [a] get-iii) crlf))

(defrule test
   (initial-fact)
   =>
   (make-instance [a] of A)
   (make-instance [b] of B))
(reset)
(run)
(clear)
(watch slots)

(defclass A (is-a USER)
  (role concrete)
  (slot foo
    (create-accessor read-write)
    (access initialize-only)))
(make-instance a of A)

(defclass B (is-a USER)
  (role concrete)
  (slot bar
     (create-accessor read-write)
     (default-dynamic (send [a] put-foo blah))))
(make-instance of B)
(unwatch slots)
(clear)                   ; DR0821

(deffunction function2 ()
  (subseq$ (create$ 3 (+ 3 1)) 1 1))

(deffunction function1 ()
   (bind ?str "")
   (bind ?result (function2))
   (loop-for-count 3
      (bind ?str (str-cat ?str " ")))) 
(loop-for-count 1000 (function1))
(clear)
(clear)                   ; DR0824

(defclass c
   (is-a USER)
   (role concrete)
   (slot s
      (access initialize-only)
      (visibility public)
      (create-accessor read-write)))

(defmessage-handler c init after
   ()
   (bind ?self:s (+ ?self:s 1)))
(make-instance of c (s 1))
(clear)                   ; DR0825

(defclass EXAMPLE
   (is-a USER)
   (role concrete)
   (slot x))
(restore-instances bug825.ins)
(clear)
(progn (release-mem) TRUE)
(clear)                   ; DR0831
(defmodule MAIN (export ?ALL))
(defmodule M (import MAIN ?ALL) (export ?ALL))
(deffunction MAIN::problem (?x))
(save "Temp//bug.clp")
(clear)
(load "Temp//bug.clp")
(clear)                   ; DR0834

(deffacts Stuff
   (Value)
   (AxisLine))  

(defrule r1
   (initial-fact)
   (not (and (Value) 
             (not (AxisLine))))
   (not (AxisLine))
   (not (Bogus))
   =>)
(reset)
(retract 2)
(run)
(clear)                   ; DR0835

(explode$
   (nth$ 1 (explode$
      (nth$ 1 (explode$
         (nth$ 1 (explode$
            (implode$ (create$
               (implode$ (create$
                  (implode$ (create$
                     (implode$ (create$ a b c)))))))))))))))
(clear)                   ; DR0837
(assert-string "()dfj )))(")
(assert-string ")(dsf")
(clear)                   ; DR0839
(ppdefclass asd)
(ppdefclass MAIN::dip)
(ppdefclass uiui::gop)
(clear)                   ; DR0840

(defmodule A
   (export deftemplate template))
(deftemplate A::template)

(defmodule B
   (import A deftemplate template))
(save "Temp//bug.tmp")
(clear)
(load "Temp//bug.tmp")
(clear)
(load mab.clp)
(save "Temp//bug.tmp")
(clear)
(load "Temp//bug.tmp")
(clear)
(load dilemma1.clp)
(save "Temp//bug.tmp")
(clear)
(load "Temp//bug.tmp")
(clear)
(load dilemma2.clp)
(save "Temp//bug.tmp")
(clear)
(load "Temp//bug.tmp")
(clear)
(load wordgame.clp)
(save "Temp//bug.tmp")
(clear)
(load "Temp//bug.tmp")
(clear)
(load zebra.clp)
(save "Temp//bug.tmp")
(clear)
(load "Temp//bug.tmp")
(clear)
(load electrnc.clp)
(load circuit3.clp)
(save "Temp//bug.tmp")
(clear)
(load "Temp//bug.tmp")
(clear)
(load mabobj.clp)
(save "Temp//bug.tmp")
(clear)
(load "Temp//bug.tmp")
(clear)
(load objfarm.clp)
(save "Temp//bug.tmp")
(clear)
(load "Temp//bug.tmp")
(clear)
(load wrdgmobj.clp)
(save "Temp//bug.tmp")
(clear)
(load "Temp//bug.tmp")
(clear)
(load wine.clp)
(save "Temp//bug.tmp")
(clear)
(load "Temp//bug.tmp")
(clear)
(clear)                   ; DR0848

(defrule test1 
   (hihi ?a $?m) 
   => 
   (progn$ (?each ?m) 
      (printout t "Value=" ?each " Index=" ?each-index crlf)))
(assert (hihi alpha bravo charli david echo))
(run)
(clear)                   ; DR0849

(defclass FRIDGE
   (is-a USER)
   (multislot contents))

(definstances test
   (fridge-1 of FRIDGE (contents a b c d)))
(reset)
(member$ (first$ (rest$ (send [fridge-1] get-contents))) (send [fridge-1] get-contents))
(member$ (first$ (rest$ (send [fridge-1] get-contents))) (rest$ (send [fridge-1] get-contents)))
(clear)                   ; DR0854

(deffunction foobar (?a ?b ?c)
   (printout t ?a " " ?b " " ?c crlf))
(funcall foobar 1)
(clear)                   ; DR0855
(fetch
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
)
(clear)                   ; DR0856
(constructs-to-c
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
1)
(clear)                   ; DR0857

(defclass
CLASSaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
(is-a OBJECT))
(profile constructs)
(profile-info)
(profile off)
(clear)                   ; DR0858

(defmodule
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)

(deffunction
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa::foo
()))
(get-deffunction-list *)
(clear)                   ; DR0867

(defrule Bad-Rule ""
   (exists (C))
   (not (B))
   =>)
(watch activations)
(reset)
(reset)
(agenda)
(unwatch activations)
(clear)                   ; DR0870

(defclass A (is-a USER)
   (slot x (default ?NONE)))
(slot-default-value A x)
(clear)                   ; DR0872
(load dr0872-1.clp)
(load dr0872-2.clp)
(clear)                   ; DR0873

(defclass A
   (is-a USER) 
   (slot x)) 

(defclass B
   (is-a USER) 
   (slot y)) 

(definstances initialization 
   (ob1 of A (x 1)) 
   (ob2 of B (y 1))) 

(defrule one 
   (object (is-a A) 
           (x ~0)) 
   (object (is-a B) 
           (y ?val)) 
   (test (> ?val 0)) 
   =>)
   
(defrule trigger
   =>
   (object-pattern-match-delay
      (modify-instance [ob1] (x 2))
      (make-instance [ob2] of B (y 2))))
(reset)
(run)
(clear)                   ; DR0874
(undefrule MAIN::)
(clear)                   ; DR0877

(deftemplate foo (multislot bar) (multislot yak))

(deffacts init
   (foo (bar) (yak)))
   
(deffunction callit (?c)
    (loop-for-count (?i ?c)
       (do-for-fact ((?f foo)) TRUE
          (bind ?b1 ?f:bar)
          (bind ?b2 ?f:yak)
          (assert (foo (bar ?b1 ?i) (yak ?b2 (- ?c ?i))))
          (retract ?f))))
      
(defrule doit
    =>
    (callit 2000))
(reset)
(run)
(clear)                   ; DR0878
(funcall assert foo a b c)
(clear)                   ; DR0879
(assert (a) (b) (c))
(implode$ (get-fact-list))
(clear)                   ; DR0880
(deftemplate matrix (slot ID) (slot JD))
(deffacts blah (matrix (ID 5) (JD 5)))
(reset)

(defrule rule1
   (matrix (ID 5) (JD 3))
   =>)

(defrule rule2
   (matrix (ID 5))
   =>)
(agenda)
(reset)
(agenda)
(clear)                   ; SourceForge 1881324: CLIPS 6.3 Beta Release 2

(deftemplate as_score 
   (slot segment_id) 
   (slot score))

(deffacts as_score_info 
   (as_score (segment_id 11)(score 5)) 
   (as_score (segment_id 12)(score 9))) 
(reset)
(facts)
(reset)
(facts)
(clear)
