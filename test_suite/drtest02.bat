(str-index "" "")                 ; DR0103 - 1
(str-index "" "a")                ; DR0103 - 2
(str-explode "")                  ; DR0104 - ()
(sub-string 1 2 "a")              ; DR0105 - "a"
(sub-string 2 2 "a")              ; DR0105 - ""
(subseq$ (create$ a) 2 2)     ; DR0106 - ()
(subseq$ (create$ a) 1 2)     ; DR0106 - (a)
(str-index "a" "aba")             ; DR0109 - 1
(str-implode)                     ; DR0112 - Error
(sub-string 1 3 "abc")            ; DR0113 - memory loss
(sub-string 1 3 "abc")            ; DR0113 - memory loss
(setgen 1)                        ; DR0114 - Error
(setgen a)                        ; DR0114 - Error
(read 1 1)                        ; DR0114 - Error
(readline 1 1)                    ; DR0114 - Error
(min)                             ; DR0114 - Error
(max)                             ; DR0114 - Error
(pi 1)                            ; DR0114 - Error
(halt 1)                          ; DR0114 - Error
(mod x y)                         ; DR0114 - Error
(setgen 1)                        ; DR0114 - Error
(gensym 10)                       ; DR0114 - Error
(if (eq 1 2) then)                ; DR0115 - Syntax
(if (eq 1 2) then else)           ; DR0115 - Syntax
(while (eq 1 2) do)               ; DR0115 - Syntax
(clear)                           ; DR0120
(defrule a 
   (x ?string) 
   => 
   (bind ?m (str-explode ?string)) 
   (bind ?str (str-implode ?m))
   (printout t ?m " " ?str crlf))
(defrule b 
   => 
   (bind ?m (str-explode "a b c 1 2 3"))
   (bind ?s (str-implode ?m))
   (printout t ?m " " ?s crlf))
(reset)                           ; DR0120
(assert (x "a b c 1 2 3"))        ; DR0120
(run)                             ; DR0120
(batch "a*a")                     ; DR0127
(open "a*a" a)                    ; DR0127
(fetch "a*a")                     ; DR0127
(clear)                           ; DR0135
(defrule rule1 ""
  (token ?level ?token-num)
  (expert ?level|* ?token-num|*)
  =>)
(assert (token 2 4))              ; DR0135
(assert (expert 2 4))             ; DR0135
(assert (expert 2 2))             ; DR0135
(assert (expert 2 *))             ; DR0135
(assert (expert * 4))             ; DR0135
(assert (expert * *))             ; DR0135
(assert (expert 4 *))             ; DR0135
(assert (expert * 2))             ; DR0135
(agenda)                          ; DR0135
(setgen 1)                        ; DR0156
(setgen -1)                       ; DR0156
(gensym)                          ; DR0156
(evenp dlaj)                      ; DR0159
(evenp 2e1)                       ; DR0159
(oddp 4.3)                        ; DR0159
(oddp 5e1)                        ; DR0159
(/ d 6)                           ; DR0160
(clear)                           ; DR0198
(deffacts bugfacts
   (base-count a 2 strand_13)
   (count-bases strand_13 [ t 3 ]))                
(defrule total
   ?accum <- (count-bases ?id $?bases-with-counts)
   (not (increment-base ? ?id ?))
   ?abase <- (base-count ?base ?count ?id)
   =>
   (retract ?accum ?abase)
   (bind ?bases-and-counts 
      (create$ [ ?base ?count ] ?bases-with-counts))
   (assert (count-bases ?id ?bases-and-counts)))
(reset)                           ; DR0198
(run)                             ; DR0198
(facts)                           ; DR0198
