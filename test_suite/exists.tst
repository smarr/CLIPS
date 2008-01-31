(set-strategy depth)
(unwatch all)
; exists.bat test
(clear)
(open "Results//exists.rsl" exists "w")
(dribble-on "Actual//exists.out")
(batch "exists.bat")
(dribble-off)
(load "compline.clp")
(printout exists "exists.bat differences are as follows:" crlf)
(compare-files "Expected//exists.out" "Actual//exists.out" exists)
; close result file
(close exists)
