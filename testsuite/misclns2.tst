(set-strategy depth)
(unwatch all)
; misclns2.bat test
(clear)
(open "Results//misclns2.rsl" misclns2 "w")
(dribble-on "Actual//misclns2.out")
(batch "misclns2.bat")
(dribble-off)
(load "compline.clp")
(printout misclns2 "misclns2.bat differences are as follows:" crlf)
(compare-files "Expected//misclns2.out" "Actual//misclns2.out" misclns2)
; close result file
(close misclns2)
