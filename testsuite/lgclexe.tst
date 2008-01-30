(set-strategy depth)
(unwatch all)
; lgclexe.clp test
(clear)
(open "Results//lgclexe.rsl" lgclexe "w")
(load "compline.clp")
(load "lgclexe.clp")
(progn (dribble-on "Actual//lgclexe.out") (test-logical) (dribble-off))
(printout lgclexe "lgclexe.clp differences are as follows:" crlf)
(compare-files "Expected//lgclexe.out" "Actual//lgclexe.out" lgclexe)
; close result file
(close lgclexe)
