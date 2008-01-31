(set-strategy depth)
(unwatch all)
; rfrshagn.clp test
(clear)
(open "Results//rfrshagn.rsl" rfrshagn "w")
(load "compline.clp")
(load "rfrshagn.clp")
(progn (dribble-on "Actual//rfrshagn.out") (testit) (dribble-off))
(printout rfrshagn "rfrshagn.clp differences are as follows:" crlf)
(compare-files "Expected//rfrshagn.out" "Actual//rfrshagn.out" rfrshagn)
; close result file
(close rfrshagn)
