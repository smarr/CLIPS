(set-strategy depth)
(unwatch all)
; rulemisc.bat test
(clear)
(open "Results//rulemisc.rsl" rulemisc "w")
(dribble-on "Actual//rulemisc.out")
(batch "rulemisc.bat")
(dribble-off)
(load "compline.clp")
(printout rulemisc "rulemisc.bat differences are as follows:" crlf)
(compare-files "Expected//rulemisc.out" "Actual//rulemisc.out" rulemisc)
; close result file
(close rulemisc)
