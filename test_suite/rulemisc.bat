(unwatch all)
(clear) ; Test Thing #1
(defrule rule-1 (foo $?b ?x) =>)
(defrule rule-2 (foo $?y) =>)
(clear) ; Test Thing #2
(watch facts)
(watch activations)
(defrule foo (not (not (and (a) (b)))) =>)
(defrule bar (not (and (a) (b))) =>)
(assert (a))
(assert (b))
(unwatch all)
(clear) ; Test Thing #3
(reset)
(defrule foo (initial-fact) (not (a)) =>)
(defrule bar (initial-fact) =>)
(agenda)
(unwatch all)
(clear) ; Test Thing #4
(defrule foo (logical (exists (a ?) (b ?))) => (assert (q)))
(reset)
(assert (a 1) (b 1) (a 2) (b 2) (a 3))
(run)
(watch facts)
(retract 1 2 3 4)
(unwatch all)
(clear) ; Test Thing #5
(defrule rule-1 (a ?x) (not (b ?x)) =>)
(reset)
(assert (a 1) (a 2) (b 2))
(run)
(refresh rule-1)
(agenda)
(clear) ; Test Thing #6
(reset)
(watch facts)
(watch activations)
(defrule all-players-practiced
   (logical (forall (player ?name)
                    (pitched ?name)
                    (batted ?name)))
   =>
   (assert (all-players-have-practiced)))
(assert (player Gary) (pitched Gary) (batted Gary))
(assert (pitched Brian) (player Brian) (batted Brian))
(run)
(retract 3)
(unwatch all)
(clear) ; Test Thing #7
(defrule rule-1
  (team ?x)
  (forall (player ?z ?x) (batted ?z) (pitched ?z))
  =>)
(matches rule-1)
(assert (team Reds))
(matches rule-1)
(assert (player Gary Reds))
(matches rule-1)
(assert (batted Gary))
(matches rule-1)
(assert (pitched Gary))
(matches rule-1)
(clear) ; Test Thing #8 - Fact Addresses References
(defrule theRule 
  ?f <- (this)
  (that ?f)
  =>)
(assert (that =(assert (this))))
(agenda)
(defrule theRule
  ?f <- (a)
  ?f <- (b)
  =>)
(defrule theRule
  (a ?f)
  ?f <- (b)
  =>)
(clear) ; Test Thing #9
(deffacts start (rule-2))
(defrule rule-1 (rule-2) (rule-2 green) =>)
(defrule rule-2 (rule-2 $?) =>)
(reset)
(agenda)
(clear) ; Test Thing #10
(defrule foo (a ?) (b ?) (c ?) =>)
(assert (a 1) (a 2) (b 1) (b 2) (c 1))
(matches foo)
(clear) ; Test Thing #11
(defrule foo 
   (exists (a ?x) (b ?x)) 
   (exists (c ?x) (d ?x))
   =>)
(reset)
(assert (a 1) (b 1) (c 2) (d 2))
(matches foo)
(clear) ; Test Thing #12

(defrule Buggy-Rule
   (A ?a)  
   (not (and (A ?a)
             (B)
             (not (and (C ?c)
                       (test (neq ?c ?a))))))
   =>) 
(reset)
(assert (A G1))
(assert (B))
(assert (C G1))
(agenda)
(clear) ; Matches
(defrule foo 
   (exists (a ?x) (b ?x) (c ?x)) 
   (exists (d ?x) (e ?x) (f ?x))
   (exists (g ?x) (h ?x) (i ?x))
   (j ?x)
   =>)
(assert (a 1) (b 1) (c 1) (d 2) (e 2) (f 2) (g 3) (h 3) (i 3) (j 4))
(matches foo)
(clear) ; Test Thing #13
(deftemplate TAG2100 (slot tag-id))
(deftemplate TAG2300 (slot parent))
(deftemplate TAG2500 (slot parent))
(deftemplate TAG2400 (slot parent))
(deftemplate GCSS-merge-tag (slot tag-id))

(defrule load-data
   =>
   (assert (TAG2300 (parent "1")))
   (assert (TAG2370))
   (assert (TAG2400 (parent "1")));  (matched no)))
   (assert (GCSS-merge-tag (tag-id "1"))))

(defrule TAG2400-AA-Update ""
   (TBX)
   (TAG2100 (tag-id ?td2))
   (TAG2500 (parent ?td2))      
   (exists (GCSS-merge-tag (tag-id ?td3))
           (TAG2400 (parent ?td2 | ?td3)) 
           (not (and (TAG2370)
                     (TAG2300 (parent ?td2 | ?td3)))))
   =>)
(reset)
(run)
(clear) ; Test Thing #14

(deftemplate TAG2100
   (slot source)
   (slot matched)
   (slot sort-order))

(defrule load-data
   =>
   (assert (TAGS100)
           (TAG2100 (source ESI) (matched yes) (sort-order 2))
           (TAG2100 (source GCSS) (matched yes) (sort-order 19))))

(defrule Rule-2 ""
   
   (TAG2100 (source ESI)
            (matched ?m))
            
   (TAG2100 (source GCSS)
            (matched ?m)
            (sort-order ?so1))

   (not (and (TAGS100)
                       
             (not (TAG2100 (source GCSS)
                           (sort-order ?so5&:(< ?so5 ?so1))))))
   
   =>)
(reset)
(run)
(clear) ; Test Thing #15
(watch activations) 
(watch facts) 
(deftemplate foo (slot bar)) 

(defrule modify-with-logical 
   (logical (something))
   ?f <- (foo (bar 1))
  => 
   (modify ?f (bar TRUE))) 

(assert (foo (bar 1)))
(assert (something))
(run) 
(facts)
(unwatch all)
(clear)
(watch activations) 
(watch facts) 
(deftemplate foo (slot bar)) 

(defrule modify-with-logical 
   (logical (something)
   ?f <- (foo (bar 1)))
  => 
   (modify ?f (bar TRUE))) 

(assert (foo (bar 1)))
(assert (something))
(run) 
(facts)
(unwatch all)
(clear) ; Test Thing #16
(watch facts)
(defrule prop
   (logical (level-search ?n))
   (not (level-search ?n1&:(> ?n1 ?n)))
   =>
   (assert (level-search (+ ?n 1))))
(reset)
(assert (level-search 1))
(run 1)
(unwatch facts)
(clear)
