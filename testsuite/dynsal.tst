(set-strategy depth)
(unwatch all)
; dynsal.clp test
(clear)
(open "Results//dynsal.rsl" dynsal "w")
(load "compline.clp")
(load "dynsal.clp")
(progn (dribble-on "Actual//dynsal.out") (testit) (dribble-off))
(printout dynsal "dynsal.clp differences are as follows:" crlf)
(compare-files "Expected//dynsal.out" "Actual//dynsal.out" dynsal)
; close result file
(close dynsal)
