(set-strategy depth)
(unwatch all)
; jnftrght.bat test
(clear)
(open "Results//jnftrght.rsl" jnftrght "w")
(dribble-on "Actual//jnftrght.out")
(batch "jnftrght.bat")
(dribble-off)
(load "compline.clp")
(printout jnftrght "jnftrght.clp differences are as follows:" crlf)
(compare-files "Expected//jnftrght.out" "Actual//jnftrght.out" jnftrght)
; close result file
(close jnftrght)
