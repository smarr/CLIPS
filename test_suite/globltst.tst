(set-strategy depth)
(unwatch all)
; globltst.clp test
(clear)
(open "Results//globltst.rsl" globltst "w")
(load "compline.clp")
(load "globltst.clp")
(reset)
(progn (dribble-on "Actual//globltst.out") (run) (dribble-off))
(printout globltst "globltst.clp differences are as follows:" crlf)
(compare-files "Expected//globltst.out" "Actual//globltst.out" globltst)
; close result file
(close globltst)
