(set-strategy depth)
(unwatch all)
; smfmmix.clp test
(clear)
(open "Results//sfmfmix.rsl" sfmfmix "w")
(load "compline.clp")
(dribble-on "Actual//sfmfmix.out")
(load "sfmfmix.clp")
(rules)
(dribble-off)
(printout sfmfmix "sfmfmix.clp differences are as follows:" crlf)
(compare-files "Expected//sfmfmix.out" "Actual//sfmfmix.out" sfmfmix)
; close result file
(close sfmfmix)
