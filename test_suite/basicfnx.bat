(clear)                            ; 10.1.1
(watch facts)                      ; 10.1.1
(assert)                           ; 10.1.1
(assert ())                        ; 10.1.1
(assert (a) ())                    ; 10.1.1
(assert () (b))                    ; 10.1.1
(assert (c) () (d))                ; 10.1.1
(assert (a ~b))                    ; 10.1.1
(assert (a &c))                    ; 10.1.1
(assert (a |d))                    ; 10.1.1
(assert (a ?x))                    ; 10.1.1
(assert (e))                       ; 10.1.1
(assert (f) (g) (h))               ; 10.1.1 
(assert (i =(+ 3 4)))              ; 10.1.1
(retract)                          ; 10.1.2
(retract 1)                        ; 10.1.2
(retract 2 4)                      ; 10.1.2
(retract 8 9)                      ; 10.1.2
(retract *)                        ; 10.1.2
(unwatch facts)                    ; 10.1.2
(clear)                            ; 10.1.5
(deftemplate foo (slot x) (slot y))
(assert-string)                    ; 10.1.5
(assert-string "a b c" "d e f")    ; 10.1.5
(assert-string 7)                  ; 10.1.5
(assert-string hello)              ; 10.1.5
(assert-string "(x y z)")          ; 10.1.5
(assert-string "(foo (y 3))")      ; 10.1.5
(facts)                            ; 10.1.5
(clear)                            ; 10.1.5
(bind)                             ; 10.1.6
(bind ?x)                          ; 10.1.6
(bind ?x 3)                        ; 10.1.6
(bind ?x 3 4)                      ; 10.1.6
(halt)                             ; 10.1.7
(halt 3)                           ; 10.1.7
