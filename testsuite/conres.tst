(set-strategy depth)
(unwatch all)
; conres.clp test
(clear)
(open "Results//conres.rsl" conres "w")
(load "compline.clp")
(load "conres.clp")
(progn (dribble-on "Actual//conres.out") (testit) (dribble-off))
(printout conres "conres.clp differences are as follows:" crlf)
(compare-files "Expected//conres.out" "Actual//conres.out" conres)
; close result file
(close conres)
