(set-strategy depth)
(unwatch all)
; incrrset.clp test
(clear)
(open "Results//incrrset.rsl" incrrset "w")
(load "compline.clp")
(load "incrrset.clp")
(progn (dribble-on "Actual//incrrset.out") (testit) (dribble-off))
(printout incrrset "incrrset.clp differences are as follows:" crlf)
(compare-files "Expected//incrrset.out" "Actual//incrrset.out" incrrset)
; close result file
(close incrrset)
