(clear)                            ; 10.9.1
(fact-index)                       ; 10.9.1
(fact-index a)                     ; 10.9.1
(fact-index 1)                     ; 10.9.1
(defglobal ?*gfi* = 1)             ; 10.9.1
(defrule foo
  ?f <- (the item)
  =>
  (bind ?*gfi* ?f)
  (printout t (fact-index ?f) crlf))
(deffacts stuff (a) (b) (c))
(reset)                            ; 10.9.1
(assert (the item))                ; 10.9.1
(run)                              ; 10.9.1
(fact-index ?*gfi*)                ; 10.9.1
(fact-index ?*gfi* ?*gfi*)         ; 10.9.1
(retract ?*gfi*)                   ; 10.9.1
(fact-index ?*gfi*)                ; 10.9.1
(clear)                            ; 10.9.1
(setgen 10)                        ; 10.9.4
(setgen)                           ; 10.9.4
(setgen 10 20)                     ; 10.9.4
(setgen a)                         ; 10.9.4
(setgen 30.3)                      ; 10.9.4
(setgen 20)                        ; 10.9.4  
(assert (gen20 gen21 gen22))       ; 10.9.4
(gensym)                           ; 10.9.2
(gensym a)                         ; 10.9.2
(gensym)                           ; 10.9.2
(gensym*)                          ; 10.9.3
(setgen 20)                        ; 10.9.3
(gensym*)                          ; 10.9.3   
(clear)                            ; 10.9.3
(setgen 20)                        ; 10.9.3
(gensym*)                          ; 10.9.3
(seed)                             ; 10.9.6
(seed a)                           ; 10.9.6
(seed 20 a)                        ; 10.9.6
(seed 30.5)                        ; 10.9.6
(seed 100)                         ; 10.9.6
(defglobal ?*r1* = 0
           ?*r2* = 0
           ?*r3* = 0)
(progn (bind ?*r1* (random))
       (bind ?*r2* (random))
       (bind ?*r3* (random))
       TRUE)
(seed 100)                         ; 10.9.5
(= ?*r1* (random))                 ; 10.9.5
(= ?*r2* (random))                 ; 10.9.5
(= ?*r3* (random))                 ; 10.9.5
(progn (random) TRUE)              ; 10.9.5
(progn (random 10) TRUE)           ; 10.9.5
(if)                               ; 10.9.7
(if (> 3 4) then)                  ; 10.9.7
(if TRUE then (+ 3 4))             ; 10.9.7
(if FALSE then (create$ a b))      ; 10.9.7
(if (create$) then 3 else 4)       ; 10.9.7
(if TRUE then a else b)            ; 10.9.7
(if FALSE then a else b)           ; 10.9.7
(if TRUE then (+ 2 1) else (+ 8 9))
(if FALSE then (+ 3 7) else (- 9 2))
(if TRUE then (printout t "1") (printout t "2" crlf)
         else (printout t "3") (printout t "4" crlf))
(if FALSE then (printout t "1") (printout t "2" crlf)
          else (printout t "3") (printout t "4" crlf))
(while)                            ; 10.9.8
(while FALSE)                      ; 10.9.8
(while FALSE do)                   ; 10.9.8
(clear)                            ; 10.9.8
(defglobal ?*t* = 10)              ; 10.9.8
(while (> ?*t* 0) do
   (printout t ?*t* "... ")
   (bind ?*t* (- ?*t* 1))
   (if (= ?*t* 0) then (printout t crlf)))
(clear)                            ; 10.9.8
(progn (time) TRUE)                ; 10.9.9
(<= (time) (time))                 ; 10.9.9
(time a)                           ; 10.9.9
(progn)                            ; 10.9.10
(progn x)                          ; 10.9.10
(progn 1 2 3)                      ; 10.9.10
(progn (create$ d) 3 (create$ b c))
(defglobal ?*v1* = 0 ?*v2* = 1)    ; 10.9.10
(progn (bind ?*v1* 3)
       (bind ?*v2* 5)
       (+ ?*v1* ?*v2*))
(clear)                            ; 10.9.11 & 10.9.12
(load "miscfnx.clp")               ; 10.9.11 & 10.9.12
(return)                           ; 10.9.11 & 10.9.12...
(return 2)
(return 1 2 3)
(return blah)
(sign 10)
(sign -5)
(sign 0)
(generic-sign 10)
(generic-sign -5)
(generic-sign 0)
(send 10 sign)
(send -5 sign)
(send 0 sign)
(break)
(break 1)
(iterate 0)
(iterate 10)
(test-return-in-while)
(while 1 do 
   (do-break)
   (printout t "Out of context break test succeeded" crlf)
   (break))
(while 1 do 
   (nested-break)
   (printout t "Nested break test succeeded" crlf)
   (break))
(while 1 do
   (return)
   (printout t "return in while test succeeded" crlf)
   (break))
(while 1 do
  (iterate (break)))
(iterate (return))
(get-function-restrictions +)
(get-function-restrictions make-instance)
(get-function-restrictions)
(get-function-restrictions bogus)
(progn (bind ?x 1) (bind ?x) ?x)
(defglobal ?*x* = (pi))
(watch globals)
(bind ?*x* 1 2 3)
(bind ?*x*)
(unwatch globals)
(progn (bind ?x abc def ghi) ?x)
(bind ?x)
(clear)
(loop-for-count (+ 3 2) do (printout t Foo crlf))
(loop-for-count -100 do (printout t "SHOULD NOT EXECUTE" crlf))
(loop-for-count)
(loop-for-count FALSE do abc)
(loop-for-count (?x 3)
  (loop-for-count (?y -4 -2) do
     (printout t (* ?x ?y) " ")
     (loop-for-count (?z -15 (* ?x ?y))
        (printout t X))
     (printout t crlf)))
(loop-for-count 1000 do (printout t Woz crlf) (break))
(deffunction foo ()
  (loop-for-count 1000 do (printout t Woz crlf) (return 4)))
(foo)
(clear)
(defglobal ?*x* = 0)  ;; CR0194
(defglobal ?*y* = 1)
(deffunction foo (?val)
  (switch ?val
     (case ?*x* then *x*)
     (case ?*y* then *y*)
     (default none)))
(foo 0)
(foo 1)
(foo 2)
(bind ?*y* 2)
(foo 2)
(switch)
(switch 1 (default))
(switch 2)
(clear)
(progn (break)) ;; CR0194 and CR0196
(progn (return))
(deffunction foo () (progn 1 2 (return)))
(foo)
(while TRUE do
  (progn (break) (printout t ERROR crlf)))
(clear)
