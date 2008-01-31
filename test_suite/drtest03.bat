(reset)                            ; DR0201
(progn (assert (red)) (retract 1)) ; DR0201
(facts)                            ; DR0201
(clear)                            ; DR0202
(deffacts stuff
   (point 1)
   (point 2)
   (point 3)
   (point 4))
(defrule get-min
   ?x <-  (point ?min)
   (not (point ?y&:(< ?y ?min)))
   ?fy <- (point ?minp1&:(<> ?min ?minp1))
   (not (point ?z&:(<> ?z ?min)&:(< ?z ?minp1)))
   =>
   (retract ?x ?fy)
   (printout t "This rule fires" crlf))
(reset)                            ; DR0202
(agenda)                           ; DR0202
(watch rules)                      ; DR0202
(run)                              ; DR0202
(unwatch rules)                    ; DR0202
(clear)                            ; DR0204
(defrule test
   ?fact <- (initial-fact)
   =>
   (printout t "any thing" crlf)
   (retract ?fact))
(reset)                            ; DR0204
(run)                              ; DR0204
(clear)                            ; DR0206
(deffacts test "rebinding of mulitfield vars"
   (_1 to see if the vars mess up if the fields are long)
   (_2 if so what is the limit also see if there is problem with bind))
(defrule ok 
   ?f1 <- (_1 $?one)
   ?f2 <- (_2 ? $?two)
   =>
   (retract ?f1 ?f2)
   (printout t "to see ... are long = " ?one  crlf)
   (printout t "if so ... with bind = "?two crlf)
   (bind ?one (create$ ?one (subseq$ ?two 1 10)))
   (printout t ?one crlf))
(reset)                            ; DR0206
(run)                              ; DR0206
(clear)                            ; DR0207
(deffacts input
   (gift ball shoe food "candies  " 3 1 )
   (but we didn't have time !))
(defrule check 
   ?f1 <- (gift ?ball $?multi)
   ?f2 <- (but $?rest)
   =>
   (printout t "?ball = "?ball crlf "?multi " ?multi crlf)
   (printout t "but " ?rest crlf)
   (printout t "let's mess with them " crlf)
   (bind ?multi (create$ (subseq$ ?rest 1 3)))
   (printout t "we didn't have = " ?multi  crlf))
(reset)                            ; DR0207
(run)                              ; DR0207
(clear)                            ; DR0232
(defrule a => (initialize-it))     ; DR0232
(reset)                            ; DR0232
(run)                              ; DR0232
(format t "%6.5 " 8655.3)          ; DR0233
(clear)                            ; DR0235
(deftemplate colors 
   (field standard (default white))
   (multifield metallic (default "plain black")))
(reset)                            ; DR0235
(assert (colors))                  ; DR0235
(assert (colors (standard black))) ; DR0235
(assert (colors (metallic "navy blue")))
(facts)                            ; DR0235
(clear)                            ; DR0238
(deftemplate auto (field name))
(defrule one 
   ?f1 <- (auto (name nil))
   =>
   (modify ?f1 (namse any))) 
(reset)                            ; DR0238
(assert (auto))                    ; DR0238
(agenda)                           ; DR0238
(run)                              ; DR0238
(format nil "%6.5f" 864)           ; DR0240
(format nil "%6.5f" 86.543)        ; DR0240
(format nil "%6.5f" 86551.)        ; DR0240    
(clear)                            ; DR0245
(deftemplate a                     ; DR0245
   (field one) (field two))
(defrule b
   (not (a (one anything) (three whatever)))
   =>)       
(clear)                            ; DR0246
(defrule error                     ; DR0246
   ?f <- (fact)
   =>
   (assert (new-fact ?f))) 
(assert (fact))                    ; DR0246
(run)                              ; DR0246
(facts)                            ; DR0246
(clear)                            ; DR0248
(deffacts a)                       ; DR0248
(clear)                            ; DR0251
(deftemplate auto (field name))    ; DR0251
(defrule one "test the naked assert & modify"
   ?f1 <- (auto (name nil))
   (not (auto (mane nil)))
   =>
   (modify ?f1 (name any)))
(str-implode (create$))          ; DR0256
(clear)                            ; DR0257
(defrule foo                       ; DR0257
   ?f1 <- (a 1) 
   ?f2 <- (b 1)
   (test (neq ?f1 ?f2))
   =>)
(defrule bar                       ; DR0257
   ?f1 <- (a ?) 
   ?f2 <- (b ?)
   (test (neq ?f1 ?f2))
   =>)
(assert (a 1) (a 2) (b 1))         ; DR0257
(agenda)                           ; DR0257
(clear)                            ; DR0261
(deffacts a (one =(reset)))        ; DR0261
(reset)                            ; DR0261
(facts)                            ; DR0261
(clear)                            ; DR0279
(defrule with-error                ; DR0279
   (value ?a&:(> ?a max))
   =>)
(defrule with-error-inside-not     ; DR0279
   (not (value ?b&:(> ?b max)))
   =>)
(clear)                            ; DR0296
(create$ / 3 1)                  ; DR0296
(eval "(create$ / 3 1)")         ; DR0296
(deffacts a (one =(eval "(create$ / 3 1)")))
(reset)                            ; DR0296
(facts)                            ; DR0296
(clear)                            ; DR0298
(deftemplate foo (field x) (field y))
(defrule this-rule-doesnt-work     ; DR0298
   (foo (y ?x1) (x ?x2&~?x1))
   =>)
(defrule this-rule-works           ; DR0298
   (foo (y ?x1&~?x2) (x ?x2))
   =>)
(assert (foo (x 3) (y 4)))         ; DR0298
(assert (foo (x 4) (y 3)))         ; DR0298
(agenda)                           ; DR0298
