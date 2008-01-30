(set-strategy depth)
(unwatch all)
; tceplace.bat test
(clear)
(open "Results//tceplace.rsl" tceplace "w")
(load "compline.clp")
(dribble-on "Actual//tceplace.out")
(batch "tceplace.bat")
(dribble-off)
(printout tceplace "tceplace.bat differences are as follows:" crlf)
(compare-files "Expected//tceplace.out" "Actual//tceplace.out" tceplace)
; close result file
(close tceplace)
