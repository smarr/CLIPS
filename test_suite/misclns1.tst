(set-strategy depth)
(unwatch all)
; misclns1.bat test
(clear)
(open "Results//misclns1.rsl" misclns1 "w")
(dribble-on "Actual//misclns1.out")
(batch "misclns1.bat")
(dribble-off)
(load "compline.clp")
(printout misclns1 "misclns1.bat differences are as follows:" crlf)
(compare-files "Expected//misclns1.out" "Actual//misclns1.out" misclns1)
; close result file
(close misclns1)
