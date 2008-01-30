(load "tceplace.clp")
(reset)
(assert (a) (d) (e) (f) (g) (h) (i) (j) (k) (l))
(agenda) ; should be foo2 foo10
(assert (b))
(agenda) ; should be foo2 foo10
(assert (c))
(agenda) ; should be foo1 foo2 foo5 foo10
