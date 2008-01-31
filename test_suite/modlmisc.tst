(set-strategy depth)
(unwatch all)
; modlmisc.bat test
(clear)
(open "Results//modlmisc.rsl" modlmisc "w")
(dribble-on "Actual//modlmisc.out")
(batch "modlmisc.bat")
(dribble-off)
(load "compline.clp")
(printout modlmisc "modlmisc.bat differences are as follows:" crlf)
(compare-files "Expected//modlmisc.out" "Actual//modlmisc.out" modlmisc)
; close result file
(close modlmisc)
