(clear)                            ; DR0301
(assert (clock =(create$ a)))    ; DR0301 
(clear)                            ; DR0301
(assert (clock =(create$ a)))    ; DR0301
(clear)                            ; DR0301
(assert (clock =(create$ a)))    ; DR0301
(clear)                            ; DR0301
(watch compile)                    ; DR0303
(subseq$)                        ; DR0305
(clear)                            ; DR0336
(deffacts initial (bounds  nil))   ; DR0336
(defrule Print                     ; DR0336
   (bounds ?type&:(or (eq ?type Cube) (eq ?type Square)))
   =>)
(reset)                            ; DR0336
(run)                              ; DR0336
(clear)                            ; DR0363
(deftemplate a                     ; DR0363
   (field one (default a a a)))    ; DR0363
(clear)                            ; DR0365
(deftemplate bar                   ; DR0365
   (field a) (multifield b))
(reset)                            ; DR0365
(assert (bar (b x y z) (a w)))     ; DR0365
(assert (bar (a g) (b =(create$ h i j))))
(facts)                            ; DR0365
(clear)                            ; DR0380
(if (= 2 2)                        ; DR0380
    then (eval (str-cat (run))))   ; DR0380
(clear)                            ; DR0381
(deftemplate a                     ; DR0381
   (field one
      (max-number-of-elements 9)
      (min-number-of-elements 3)))
(clear)                            ; DR0382
(deftemplate a                     ; DR0382
   (field one (default nothing)))  ; DR0382
(assert (a (one a a a a)))         ; DR0382
(clear)                            ; DR0383
(deftemplate data (field one) (field two))
(assert (data (one)))              ; DR0383
(clear)                            ; DR0396
(create$)                        ; DR0396
(str-compare (str-cat (create$)) 
             (str-cat (create$)))
(clear)                            ; DR0397
(assert-string (str-cat " (" (bind ?var (read)) ")"))
anything
(facts)                            ; DR0397
