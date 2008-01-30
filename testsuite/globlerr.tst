(unwatch all)
; globlerr.clp test
(clear)
(open "Results//globlerr.rsl" globlerr "w")
(load "compline.clp")
(dribble-on "Actual//globlerr.out")
(load "globlerr.clp")
(show-defglobals)
(dribble-off)
(printout globlerr "globlerr.clp differences are as follows:" crlf)
(compare-files "Expected//globlerr.out" "Actual//globlerr.out" globlerr)
; close result file
(close globlerr)
