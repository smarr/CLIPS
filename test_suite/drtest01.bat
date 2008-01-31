(toss "a")                        ; DR0001
(toss a)                          ; DR0001
(clear)                           ; DR0003
(>= 1)                            ; DR0003
(defrule b (x ?y&:(>= 1)) =>)     ; DR0003
(clear)                           ; DR0006
(defrule x                        ; DR0006
   (not (a ?x))
   (not (b ?x))
   =>)
(str-cat (readline))              ; DR0011
a b
(clear)                           ; DR0012
(defrule b                        ; DR0012
   (not (z))
   (test (> 0 1))
   =>)
(agenda)                          ; DR0012
(reset)                           ; DR0012
(agenda)                          ; DR0012
(clear)                           ; DR0033
(=)                               ; DR0033
(defrule b (x ?v) (test (=)) =>)  ; DR0033
(defrule c (x ?v) => (clear))     ; DR0033
(assert (x 9))                    ; DR0033
(run)                             ; DR0033
(clear)                           ; DR0034
(bind ?b)                         ; DR0034
(defrule a (x ?b) => (bind ?b)))  ; DR0034
(assert (x n))                    ; DR0034
(run)                             ; DR0034
(close blah)                      ; DR0037
(deffacts a ())                   ; DR0039
(assert ())                       ; DR0039
(system)                          ; DR0043
(clear)                           ; DR0047
(defrule a (= 5 7) =>)            ; DR0047
(assert (= 5 7))                  ; DR0047
(agenda)                          ; DR0047
(open "Temp//f1.tmp" name "w")    ; DR0048
(open "Temp//f2.tmp" name "w")    ; DR0048
(close name)                      ; DR0048
(clear)                           ; DR0051
(defrule a                        ; DR0051
   (data $?a)
   =>
   (format t "%s" ?a))
(assert (data a b c d))           ; DR0051
(run)                             ; DR0051
(clear)                           ; DR0052
(defrule one                      ; DR0052
   (b ?x ?y&:(evenp ?value)|?y&:(oddp ?value) ?z)
   =>)
(assert (b 2 3 4))                ; DR0052
(agenda)                          ; DR0052
(clear)                           ; DR0053
(defrule rule1 (oven-type $?) =>) ; DR0053
(defrule rule2                    ; DR0053
  (oven-type ?ch&:(not (numberp ?ch)))
  =>)
(defrule rule3 (oven-type ?) =>)  ; DR0053
(assert (oven-type 1))            ; DR0053
(agenda)                          ; DR0053
(clear)                           ; DR0054
(defrule test (a ?i) (b ?n&=?i)   ; DR0054
  =>)
(assert (a 3) (b 3))              ; DR0054
(agenda)                          ; DR0054
(oddp 3.1)                        ; DR0056
(oddp 4.1)                        ; DR0056
(evenp 3.1)                       ; DR0056
(evenp 4.1)                       ; DR0056
(integerp a)                      ; DR0058
(symbolp 3)                       ; DR0059
(symbolp x)                       ; DR0059
(symbolp "x")                     ; DR0059
(clear)                           ; DR0063
(defrule foo                      ; DR0063
  (data foo $?x)
  =>
  (printout t (nth 3 ?x) crlf))  ; DR0063
(assert (data foo a b here d))    ; DR0063
(run)                             ; DR0063
(retract ?f)                      ; DR0067
(clear)                           ; DR0068
(defrule foo =>)                  ; DR0068
(reset)                           ; DR0068
(agenda)                          ; DR0068
(undefrule foo)                   ; DR0068
(agenda)                          ; DR0068
(clear)                           ; DR0069
(defrule foo ?f (fact) =>)        ; DR0069
(defrule foo ?f <- fact)          ; DR0069
(deffacts info (fact 1) fact 2)   ; DR0070
(** 3 2)                          ; DR0074
(clear)                           ; DR0075
(assert (a~b))                    ; DR0075
(facts)                           ; DR0075
(print-region t "f.f")            ; DR0077
(printout q)                      ; DR0080
(eq)                              ; DR0082
(neq)                             ; DR0082
(neq 1 1 2)                       ; DR0082
(!= 1 1 2)                        ; DR0082
(and)                             ; DR0083
(or)                              ; DR0083
(not)                             ; DR0083
(clear)                           ; DR0087
(defrule a                        ; DR0087
  (x $?y) 
  => 
  (assert (z ?y)))
(assert (x q))                    ; DR0087
(run)                             ; DR0087
(facts)                           ; DR0087
(subseq$ a 2 4)                 ; DR0090
(sub-string)                      ; DR0092
(str-explode "a b c")             ; DR0093
(length (str-explode "a b c"))    ; DR0093
(tan 0)                           ; DR0096
(sec 0)                           ; DR0096
(/ 1 0)                           ; DR0097
(/ 1.0 0.0)                       ; DR0097 
(** 0 0)                          ; DR0098
(** 0 -.5)                        ; DR0098
(** -2 .5)                        ; DR0098 
(** 0.0 0.0)                      ; DR0098
(** 0.0 -.5)                      ; DR0098
(** -2.0 .5)                      ; DR0098
(** -3 2)                         ; DR0098
(** -3 2.2)                       ; DR0098
