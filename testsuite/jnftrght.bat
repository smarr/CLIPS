(clear)      ; Test logicals
(watch activations)
(watch facts)

(defrule r1
   (logical (exists (a ?) (b ?)))
   (c ?)
   =>
   (assert (yaba)))

(defrule r2
   (exists (a ?) (b ?))
   (c ?)
   =>
   (assert (daba)))

(defrule r3
   (logical (forall (a ?) (b ?)))
   (c ?)
   =>
   (assert (doo)))
(reset)
(assert (a 1) (b 1) (a 2) (b 2) (c 1))
(run)
(retract 5)  ; (c 1)
(retract 1)  ; (a 1)
(retract 3)  ; (a 2)
(retract 2)  ; (b 1)
(retract 4)  ; (b 2)
(assert (a 3))
(unwatch all)
(clear)      ; Test logical CE within not CE error
(defrule r1 "error" (exists (logical (a ?) (b ?))) =>)
(defrule r2 "error" (forall (logical (a ?)) (b ?) (c ?)) =>)
(defrule r3 "error" (not (logical (a ?) (b ?))) =>) 
(clear)      ; Test forall CE

(defrule all-trained
   (forall (player ?x)
           (batted ?x)
           (pitched ?x))
   =>)
(unwatch all)
(watch activations)
(watch facts)
(reset)
(agenda)
(assert (player Gary))
(assert (pitched Gary))
(assert (batted Gary))
(assert (player Brian))
(assert (player Tom))
(assert (pitched Tom))
(assert (pitched Brian))
(assert (batted Tom))
(assert (batted Brian))
(retract 8)
(retract 5)
(reset)
(agenda)
(assert (player Gary))   ;1
(assert (pitched Gary))  ;2
(assert (batted Gary))   ;3
(assert (player Tom))    ;4
(assert (pitched Tom))   ;5
(assert (batted Tom))    ;6
(retract 6)
(retract 4)
(reset)
(agenda)
(assert (player Gary))   ;1
(assert (pitched Gary))  ;2
(assert (batted Gary))   ;3
(retract 3)
(retract 1)
(clear)
(unwatch all)

(defrule rule-1
  (declare (salience 5))
  (forall (a ?x) (b ?x) (c ?x))
  =>)

(defrule rule-2
  (declare (salience 4))
  (exists (a ?x) (b ?x) (c ?x))
  =>)

(defrule rule-3
  (declare (salience 3))
  (not (and (a ?x) (b ?x)))
  =>)

(defrule rule-4
  (declare (salience 2))
  (not (and (b ?x) (d ?x)))
  =>)

(defrule rule-5
  (declare (salience 1))
  (forall (d ?x) (e ?x) (f ?x))
  =>)
(reset)
(agenda)
(assert (a 1) (a 2) (a 3))
(agenda)
(assert (b 1) (b 2))
(agenda)
(assert (c 2) (c 3) (c 4) (c 5))
(agenda)
(assert (d 3) (d 4))
(agenda)
(assert (e 3) (e 4))
(agenda)
(assert (f 3) (f 4))
(agenda)
(assert (b 3) (c 1))
(agenda)
(retract 2 3 4)
(agenda)
(assert (d 2))
(agenda)
(retract 10)
(agenda)
(retract 1)
(agenda)
(retract 5 6 7 8 9)
(agenda)
(retract 11 13 15)
(agenda)
(retract 18)
(agenda)
(retract 12 14 16 17)
(agenda)
(clear) ; Test combinations of variable use

(defrule foo-1
  (a ?x)
  (not (and (b ?x) (c ?x) (d ?x)))
  (e ?x)
  (f ?x)
  =>)
(reset)
(assert (a 3) (e 3) (f 3))
(agenda) ;; Should have activations
(assert (b 4) (c 4) (d 4))
(agenda) ;; Should have activations
(assert (b 3) (c 3) (d 3))
(agenda) ;; Should have no activations
(retract 1 2 3)
(agenda) ;; Should have no activations
(clear)

(defrule foo-2
  (a ?x)
  (not (and (b ?x) (c ?x) (d ?x)))
  (not (and (e ?x) (f ?x) (g ?x)))
  (h ?x)
  (i ?x)
  =>)
(reset)
(assert (a 3) (h 3) (i 3))
(agenda) ;; Should have activations
(assert (b 4) (c 4) (d 4))
(agenda) ;; Should have activations
(assert (e 5) (f 5) (g 5))
(agenda) ;; Should have activations
(assert (b 3) (c 3) (d 3))
(agenda) ;; Should have no activations
(assert (e 3) (f 3) (g 3))
(agenda) ;; Should have no activations
(retract 10 11 12)
(agenda) ;; Should have no activations
(clear)

(defrule foo-3
  (a ?x)
  (b ?x)
  (not (and (c ?x) (d ?x)))
  (e ?x)
  =>)
(reset)
(assert (a 3) (b 3) (e 3))
(agenda) ;; Should have activations
(assert (c 4) (d 4))
(agenda) ;; Should have activations
(assert (c 3) (d 3))
(agenda) ;; Should have no activations
(retract 1 2 3)
(agenda) ;; Should have no activations
(clear)

(defrule foo-4
  (a ?x)
  (not (and (b ?x) 
            (c ?x)
            (not (and (e ?x) (f ?x) (g ?x)))
            (h ?x)))
  (i ?x)
  =>)
(reset)
(assert (a 3) (i 3))
(agenda) ;; Should have activations
(assert (b 4) (c 4) (h 4))
(agenda) ;; Should have activations
(assert (e 4) (f 4) (g 4))
(agenda) ;; Should have activations
(assert (b 3) (c 3) (h 3))
(agenda) ;; Should have no activations
(assert (e 3) (f 3) (g 3))
(agenda) ;; Should have activations
(retract 9 10 11)
(agenda) ;; Should have activations
(clear)

(defrule foo-5
  (a ?x)
  (not (and (b ?) 
            (c ?x)
            (not (and (d ?) (e ?) (f ?)))
            (g ?x)))
  (h ?x)
  =>)
(reset)
(assert (a 3) (h 3))
(agenda) ;; Should have activations
(assert (b 4) (c 4) (g 4))
(agenda) ;; Should have activations
(assert (b 3) (c 3) (g 3))
(agenda) ;; Should have no activations
(assert (d 1) (e 2) (f 3))
(agenda) ;; Should have activations
(retract 3 4 5)
(agenda) ;; Should have activations
(clear)

(defrule foo-6
  (a ?)
  (not (and (b ?) 
            (c ?)
            (not (and (d ?) 
                      (e ?x)))
            (f ?x)))
  (g ?)
  =>)
(reset)
(assert (a 3) (g 3))
(agenda) ;; Should have activations
(assert (b 4) (c 4) (f 4))
(agenda) ;; Should have no activations
(assert (d 1) (e 3))
(agenda) ;; Should have activations
(assert (e 4))
(agenda) ;; Should have activations
(clear) ;; Test not/or
(defrule foo (not (or (a) (b))) =>)
(reset)
(agenda)
(assert (a))
(agenda)
(assert (b))
(agenda)
(retract 1)
(agenda)
(retract 2)
(agenda)
(clear) ; New Indexing Code
(watch activations)

(defrule foo
   (a ?x ?y ?z)
   (not (and (b ?x) (c ?y) (d ?z)))
   =>)
(assert (a 1 2 3))
(assert (b 1))
(assert (c 2))
(assert (d 3))
(clear)
(deftemplate a (slot x) (slot y) (slot z))
(deftemplate b (slot x))
(deftemplate c (slot y)) 
(deftemplate d (slot z))

(defrule foo 
   (a (x ?x) (y ?y) (z ?z))
   (not (and (b (x ?x))
             (c (y ?y))
             (d (z ?z))))
   =>)
(assert (a (x 1) (y 2) (z 3)))
(assert (b (x 1)))
(assert (c (y 2)))
(assert (d (z 3)))
(clear)
(deftemplate a (multislot x) (multislot y) (multislot z))
(deftemplate b (multislot x))
(deftemplate c (multislot y)) 
(deftemplate d (multislot z))

(defrule foo 
   (a (x $? $? ?x) (y $? $? ?y) (z $? $? ?z))
   (not (and (b (x $? $? ?x))
             (c (y $? $? ?y))
             (d (z $? $? ?z))))
   =>)
(assert (a (x 1) (y 2) (z 3)))
(assert (b (x 1)))
(assert (c (y 2)))
(assert (d (z 3)))
(clear)

(defrule foo
   (a ?x)
   (b ?y)
   (not (and (c ?z)
             (d ?w)
             (test (and (> ?x ?z) (< ?y ?w)))))
   =>)
(assert (a 3) (b 5))
(assert (c 1))
(assert (d 6))
(clear)

(defrule problem-rule-1
   (A)
   (not (and (B)
             (not (and (C) (D)))))
   (not (E))
   =>)
(clear)

(defrule problem-rule-2
   (A)  
   (not (and (B)
             (not (and (C) (D)))))

   (not  (and (E)   
              (F)))
   =>)
(clear)

(defrule problem-rule-3
   (A)  
   (exists (or (and (B) 
                    (C))  
               (and (D)
                    (E))
               (and (F)
                    (G)
                    (exists (H)
                            (I)
                            (J))
                    (K)
                    (L)
                    (exists (M)
                            (N)
                            (O)))
               (and (P)
                    (Q)
                    (R))
               (and (S)   
                    (T))))
   (not (U))
   =>)
(clear)

(defrule problem-rule-4
   (A ?td2)  
   (not (and (not (and (B) 
                       (C ?td2)))
             (not (D))))
   =>)
(clear)

(defrule buggy-rule
   (A ?td2)
   (not (and (B) 
             (not (and (C ?x)
                       (exists (D ?td2)
                               (E))))))
   =>)
(reset)
(assert (B))
(assert (A 3))
(agenda)
(clear)

(defrule problem-rule-5
   (A ?td2)  
   (not (and (not (and (B) 
                       (C ?td2)))  
             (not (and (D)
                       (not (E ?td2))))))
   =>)

(clear)
(deftemplate select-GCSS-TAG2100 
   (slot tag-id) 
   (slot process-order))

(defrule TAG2100-New-2.5.2.6.2
   
   (X)
   
   (exists (and (select-GCSS-TAG2100 (tag-id ?tag-id))
   
                (exists (select-GCSS-TAG2100 (tag-id ~?tag-id)))))   

   =>)
   
(defrule  TAG2100-Use-2.5.2.6.3
      
   (Y)
      
  (select-GCSS-TAG2100 (tag-id ?tag-id))
            
  (exists (select-GCSS-TAG2100 (tag-id ~?tag-id))) 
                                        
  =>)
(assert (select-GCSS-TAG2100 (tag-id 2) (process-order 2)))
(assert (select-GCSS-TAG2100 (tag-id 2) (process-order 3)))
(assert (select-GCSS-TAG2100 (tag-id 1) (process-order 5)))
(clear)

(deftemplate TAG2300
   (slot matched)
   (slot action)
   (slot tag-id))

(deftemplate TAG2370
   (slot parent))

(deftemplate TAG2400
   (slot action))

(defrule rule-3
   (TAG2400 (action ?))      
   (exists (and (TAG2370 (parent ?equipment))
            
                (TAG2300 (tag-id ?equipment)
                         (matched ~no))))
   =>)
(reset)
(assert (TAG2370 (parent "GCSS-E2")))
(assert (TAG2370 (parent "GCSS-E3")))
(assert (TAG2400 (action none)))
(assert (TAG2300 (matched yes) (action none) (tag-id "GCSS-E2")))
(assert (TAG2300 (matched maybe) (action none) (tag-id "GCSS-E3")))
(retract 4)
(assert (TAG2300 (matched yes) (action update) (tag-id "GCSS-E2")))
(retract 3)
(assert (TAG2400 (action delete)))
(clear)
