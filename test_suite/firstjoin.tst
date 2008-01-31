(set-strategy depth)
(unwatch all)
; firstjoin.bat test
(clear)
(open "Results//firstjoin.rsl" firstjoin "w")
(dribble-on "Actual//firstjoin.out")
(batch "firstjoin.bat")
(dribble-off)
(load "compline.clp")
(printout firstjoin "firstjoin.bat differences are as follows:" crlf)
(compare-files "Expected//firstjoin.out" "Actual//firstjoin.out" firstjoin)
; close result file
(close firstjoin)
