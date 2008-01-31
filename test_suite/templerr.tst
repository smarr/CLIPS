(unwatch all)
; templerr.clp test
(clear)
(open "Results//templerr.rsl" templerr "w")
(load "compline.clp")
(dribble-on "Actual//templerr.out")
(load "templerr.clp")
(list-deftemplates)
(dribble-off)
(printout templerr "templerr.clp differences are as follows:" crlf)
(compare-files "Expected//templerr.out" "Actual//templerr.out" templerr)
; close result file
(close templerr)
