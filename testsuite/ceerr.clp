
(defrule error-1 
  (a)
  (test (> ?x ?y))
  =>)

(defrule error-2 ""
  (not (a))
  (test (> ?x ?y))
  =>)

(defrule error-3 
  (not (a))
  (test (> 5 3))
  (foo :(> ?x ?y))
  =>)

(defrule error-4 
  (not (a))
  (test (> 5 3))
  (foo)
  (test (> ?x ?y))
  =>)

(defrule error-5 ""
  (not (a b&:(> ?x ?y)))
  =>)

(defrule error-6 ""
  (not (and (test (> 5 3))
            (not (and (test (> 5 3))
                      (not (and (test (> ?x ?y))))))))
  =>)

(defrule error-7 ""
  (not (and (test (> 5 3))
            (not (and (test (> 5 3))
                      (not (and (test (> 5 4))
                                (a ~?x)))))))
  =>)

(defrule error-8 
  (not (a))
  (test (> 5 3))
  (foo)
  (test (> 5 3))
  (foo :(> ?x ?y))
  =>)

(defrule error-9
  (exists)
  =>)
  
(defrule error-10
  (forall)
  =>)
  
(defrule error-11
  (forall (a))
  =>)
