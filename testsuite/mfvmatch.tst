(set-strategy depth)
(unwatch all)
; mfvmatch.clp test
(clear)
(load "mfvmatch.clp")
(progn (dribble-on "Actual//mfvmatch.out") (reset) (run) (dribble-off))
(load compline.clp)
(open "Results//mfvmatch.rsl" mfvmatch "w")
(printout mfvmatch "mfvmatch.clp differences are as follows:" crlf)
(compare-files "Expected//mfvmatch.out" "Actual//mfvmatch.out" mfvmatch)
; close result file
(close mfvmatch)
