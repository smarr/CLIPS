(set-strategy depth)
(unwatch all)
; fldval50.clp test
(clear)
(open "Results//fldval50.rsl" fldval50 "w")
(load "compline.clp")
(load "fldval50.clp")
(reset)
(progn (dribble-on "Actual//fldval50.out") (run))
7
(progn (run) (dribble-off))
(printout fldval50 "fldval50.clp differences are as follows:" crlf)
(compare-files "Expected//fldval50.out" "Actual//fldval50.out" fldval50)
; close result file
(close fldval50)
