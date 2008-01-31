;;; ***************************************************
;;;   COOL MESSAGE DISPATCH TESTS

;;; To execute this test, load this file, then
;;; execute the command (test-message-dispatch).
;;; ***************************************************

(defglobal ?*result* = "")
(defglobal ?*around-shadowp* = TRUE)
(defglobal ?*primary-shadowp* = TRUE)

(defclass X (is-a USER)
   (role concrete)
   (slot X-slot (create-accessor read-write) (default XP1XP2)))

(defclass Y (is-a X)
   (slot Y-slot (create-accessor read-write) (default YP1YP2)))

(defclass Z (is-a Y)
   (slot Z-slot (create-accessor read-write) (default ZP1ZP2)))

(definstances test-instances
  (x of X)
  (y of Y)
  (z of Z))

;;; Class X Message-handlers

(defmessage-handler X test around ()
  (bind ?*result* (sym-cat ?*result* XA1))
  (if (and ?*around-shadowp* (next-handlerp))  then
     (call-next-handler))
  (bind ?*result* (sym-cat ?*result* XA2)))

(defmessage-handler X test before ()
  (bind ?*result* (sym-cat ?*result* XB)))

(defmessage-handler X test primary ()
  (bind ?*result* (sym-cat ?*result* XP1))
  (if (and ?*primary-shadowp* (next-handlerp))  then
     (call-next-handler))
  (bind ?*result* (sym-cat ?*result* XP2)))

(defmessage-handler X test after ()
  (bind ?*result* (sym-cat ?*result* XF)))

(defmessage-handler X get-X-slot around ()
  (bind ?*result* (sym-cat ?*result* XA1))
  (bind ?rtn "")
  (if (and ?*around-shadowp* (next-handlerp))  then
     (bind ?rtn (call-next-handler)))
  (bind ?*result* (sym-cat ?*result* XA2))
  ?rtn)

(defmessage-handler X get-X-slot before ()
  (bind ?*result* (sym-cat ?*result* XB)))

(defmessage-handler X get-X-slot after ()
  (bind ?*result* (sym-cat ?*result* XF)))

(defmessage-handler X get-Y-slot around ()
  (bind ?*result* (sym-cat ?*result* XA1))
  (bind ?rtn "")
  (if (and ?*around-shadowp* (next-handlerp))  then
     (bind ?rtn (call-next-handler)))
  (bind ?*result* (sym-cat ?*result* XA2))
  ?rtn)

(defmessage-handler X get-Y-slot before ()
  (bind ?*result* (sym-cat ?*result* XB)))

(defmessage-handler X get-Y-slot primary ()
  (bind ?*result* (sym-cat ?*result* XP1))
  (bind ?rtn "")
  (if (and ?*primary-shadowp* (next-handlerp))  then
     (bind ?rtn (call-next-handler)))
  (bind ?*result* (sym-cat ?*result* XP2))
  ?rtn)

(defmessage-handler X get-Y-slot after ()
  (bind ?*result* (sym-cat ?*result* XF)))

(defmessage-handler X get-Z-slot around ()
  (bind ?*result* (sym-cat ?*result* XA1))
  (bind ?rtn "")
  (if (and ?*around-shadowp* (next-handlerp))  then
     (bind ?rtn (call-next-handler)))
  (bind ?*result* (sym-cat ?*result* XA2))
  ?rtn)

(defmessage-handler X get-Z-slot before ()
  (bind ?*result* (sym-cat ?*result* XB)))

(defmessage-handler X get-Z-slot primary ()
  (bind ?*result* (sym-cat ?*result* XP1))
  (bind ?rtn "")
  (if (and ?*primary-shadowp* (next-handlerp))  then
     (bind ?rtn (call-next-handler)))
  (bind ?*result* (sym-cat ?*result* XP2))
  ?rtn)

(defmessage-handler X get-Z-slot after ()
  (bind ?*result* (sym-cat ?*result* XF)))

;;; Class Y Message-handlers

(defmessage-handler Y test around ()
  (bind ?*result* (sym-cat ?*result* YA1))
  (if (and ?*around-shadowp* (next-handlerp))  then
     (call-next-handler))
  (bind ?*result* (sym-cat ?*result* YA2)))

(defmessage-handler Y test before ()
  (bind ?*result* (sym-cat ?*result* YB)))

(defmessage-handler Y test primary ()
  (bind ?*result* (sym-cat ?*result* YP1))
  (if (and ?*primary-shadowp* (next-handlerp))  then
     (call-next-handler))
  (bind ?*result* (sym-cat ?*result* YP2)))

(defmessage-handler Y test after ()
  (bind ?*result* (sym-cat ?*result* YF)))

(defmessage-handler Y get-X-slot around ()
  (bind ?*result* (sym-cat ?*result* YA1))
  (bind ?rtn "")
  (if (and ?*around-shadowp* (next-handlerp))  then
     (bind ?rtn (call-next-handler)))
  (bind ?*result* (sym-cat ?*result* YA2))
  ?rtn)

(defmessage-handler Y get-X-slot before ()
  (bind ?*result* (sym-cat ?*result* YB)))

(defmessage-handler Y get-X-slot primary ()
  (bind ?*result* (sym-cat ?*result* YP1))
  (bind ?rtn "")
  (if (and ?*primary-shadowp* (next-handlerp))  then
     (bind ?rtn (call-next-handler)))
  (bind ?*result* (sym-cat ?*result* YP2))
  ?rtn)

(defmessage-handler Y get-X-slot after ()
  (bind ?*result* (sym-cat ?*result* YF)))

(defmessage-handler Y get-Y-slot around ()
  (bind ?*result* (sym-cat ?*result* YA1))
  (bind ?rtn "")
  (if (and ?*around-shadowp* (next-handlerp))  then
     (bind ?rtn (call-next-handler)))
  (bind ?*result* (sym-cat ?*result* YA2))
  ?rtn)

(defmessage-handler Y get-Y-slot before ()
  (bind ?*result* (sym-cat ?*result* YB)))

(defmessage-handler Y get-Y-slot after ()
  (bind ?*result* (sym-cat ?*result* YF)))

(defmessage-handler Y get-Z-slot around ()
  (bind ?*result* (sym-cat ?*result* YA1))
  (bind ?rtn "")
  (if (and ?*around-shadowp* (next-handlerp))  then
     (bind ?rtn (call-next-handler)))
  (bind ?*result* (sym-cat ?*result* YA2))
  ?rtn)

(defmessage-handler Y get-Z-slot before ()
  (bind ?*result* (sym-cat ?*result* YB)))

(defmessage-handler Y get-Z-slot primary ()
  (bind ?*result* (sym-cat ?*result* YP1))
  (bind ?rtn "")
  (if (and ?*primary-shadowp* (next-handlerp))  then
     (bind ?rtn (call-next-handler)))
  (bind ?*result* (sym-cat ?*result* YP2))
  ?rtn)

(defmessage-handler Y get-Z-slot after ()
  (bind ?*result* (sym-cat ?*result* YF)))

;;; Class Z Message-handlers

(defmessage-handler Z test around ()
  (bind ?*result* (sym-cat ?*result* ZA1))
  (if (and ?*around-shadowp* (next-handlerp))  then
     (call-next-handler))
  (bind ?*result* (sym-cat ?*result* ZA2)))

(defmessage-handler Z test before ()
  (bind ?*result* (sym-cat ?*result* ZB)))

(defmessage-handler Z test primary ()
  (bind ?*result* (sym-cat ?*result* ZP1))
  (if (and ?*primary-shadowp* (next-handlerp))  then
     (call-next-handler))
  (bind ?*result* (sym-cat ?*result* ZP2)))

(defmessage-handler Z test after ()
  (bind ?*result* (sym-cat ?*result* ZF)))

(defmessage-handler Z get-X-slot around ()
  (bind ?*result* (sym-cat ?*result* ZA1))
  (bind ?rtn "")
  (if (and ?*around-shadowp* (next-handlerp))  then
     (bind ?rtn (call-next-handler)))
  (bind ?*result* (sym-cat ?*result* ZA2))
  ?rtn)

(defmessage-handler Z get-X-slot before ()
  (bind ?*result* (sym-cat ?*result* ZB)))

(defmessage-handler Z get-X-slot primary ()
  (bind ?*result* (sym-cat ?*result* ZP1))
  (bind ?rtn "")
  (if (and ?*primary-shadowp* (next-handlerp))  then
     (bind ?rtn (call-next-handler)))
  (bind ?*result* (sym-cat ?*result* ZP2))
  ?rtn)

(defmessage-handler Z get-X-slot after ()
  (bind ?*result* (sym-cat ?*result* ZF)))

(defmessage-handler Z get-Y-slot around ()
  (bind ?*result* (sym-cat ?*result* ZA1))
  (bind ?rtn "")
  (if (and ?*around-shadowp* (next-handlerp))  then
     (bind ?rtn (call-next-handler)))
  (bind ?*result* (sym-cat ?*result* ZA2))
  ?rtn)

(defmessage-handler Z get-Y-slot before ()
  (bind ?*result* (sym-cat ?*result* ZB)))

(defmessage-handler Z get-Y-slot primary ()
  (bind ?*result* (sym-cat ?*result* ZP1))
  (bind ?rtn "")
  (if (and ?*primary-shadowp* (next-handlerp))  then
     (bind ?rtn (call-next-handler)))
  (bind ?*result* (sym-cat ?*result* ZP2))
  ?rtn)

(defmessage-handler Z get-Y-slot after ()
  (bind ?*result* (sym-cat ?*result* ZF)))

(defmessage-handler Z get-Z-slot around ()
  (bind ?*result* (sym-cat ?*result* ZA1))
  (bind ?rtn "")
  (if (and ?*around-shadowp* (next-handlerp))  then
     (bind ?rtn (call-next-handler)))
  (bind ?*result* (sym-cat ?*result* ZA2))
  ?rtn)

(defmessage-handler Z get-Z-slot before ()
  (bind ?*result* (sym-cat ?*result* ZB)))

(defmessage-handler Z get-Z-slot after ()
  (bind ?*result* (sym-cat ?*result* ZF)))

(deffunction print-dispatch-test 
  (?test-num ?asp ?psp ?ins ?msg ?test-result ?test-rtn)
  (bind ?*around-shadowp* ?asp)
  (bind ?*primary-shadowp* ?psp)
  (bind ?*result* "")
  (bind ?rtn (send ?ins ?msg))
  (if (and (eq ?*result* ?test-result) (eq ?rtn ?test-rtn)) then
     (printout t "DISPATCH TEST #" ?test-num " OK." crlf)
   else
     (printout t "DISPATCH TEST #" ?test-num " BAD." crlf)))

(deffunction testit ()
  (reset)
  (print-dispatch-test 1 TRUE TRUE [x] test
                  XA1XBXP1XP2XFXA2
                  XA1XBXP1XP2XFXA2)
  (print-dispatch-test 2 TRUE TRUE [y] test
                  YA1XA1YBXBYP1XP1XP2YP2XFYFXA2YA2
                  YA1XA1YBXBYP1XP1XP2YP2XFYFXA2YA2)
  (print-dispatch-test 3 TRUE TRUE [z] test
                  ZA1YA1XA1ZBYBXBZP1YP1XP1XP2YP2ZP2XFYFZFXA2YA2ZA2
                  ZA1YA1XA1ZBYBXBZP1YP1XP1XP2YP2ZP2XFYFZFXA2YA2ZA2)
  (print-dispatch-test 4 TRUE FALSE [x] test
                  XA1XBXP1XP2XFXA2
                  XA1XBXP1XP2XFXA2)
  (print-dispatch-test 5 TRUE FALSE [y] test
                  YA1XA1YBXBYP1YP2XFYFXA2YA2
                  YA1XA1YBXBYP1YP2XFYFXA2YA2)
  (print-dispatch-test 6 TRUE FALSE [z] test
                  ZA1YA1XA1ZBYBXBZP1ZP2XFYFZFXA2YA2ZA2
                  ZA1YA1XA1ZBYBXBZP1ZP2XFYFZFXA2YA2ZA2)
  (print-dispatch-test 7 FALSE TRUE [x] test XA1XA2 XA1XA2)
  (print-dispatch-test 8 FALSE TRUE [y] test YA1YA2 YA1YA2)
  (print-dispatch-test 9 FALSE TRUE [z] test ZA1ZA2 ZA1ZA2)
  (print-dispatch-test 10 FALSE FALSE [x] test XA1XA2 XA1XA2)
  (print-dispatch-test 11 FALSE FALSE [y] test YA1YA2 YA1YA2)
  (print-dispatch-test 12 FALSE FALSE [z] test ZA1ZA2 ZA1ZA2)
  (print-dispatch-test 13 TRUE TRUE [x] get-X-slot XA1XBXFXA2 XP1XP2)
  (print-dispatch-test 14 TRUE TRUE [x] get-Y-slot XA1XBXP1XP2XFXA2 "")
  (print-dispatch-test 15 TRUE TRUE [x] get-Z-slot XA1XBXP1XP2XFXA2 "")
  (print-dispatch-test 16 TRUE TRUE [y] get-X-slot 
     YA1XA1YBXBYP1YP2XFYFXA2YA2 XP1XP2)
  (print-dispatch-test 17 TRUE TRUE [y] get-Y-slot 
     YA1XA1YBXBXFYFXA2YA2 YP1YP2)
  (print-dispatch-test 18 TRUE TRUE [y] get-Z-slot 
     YA1XA1YBXBYP1XP1XP2YP2XFYFXA2YA2 "")
  (print-dispatch-test 19 TRUE TRUE [z] get-X-slot 
     ZA1YA1XA1ZBYBXBZP1YP1YP2ZP2XFYFZFXA2YA2ZA2 XP1XP2)
  (print-dispatch-test 20 TRUE TRUE [z] get-Y-slot 
     ZA1YA1XA1ZBYBXBZP1ZP2XFYFZFXA2YA2ZA2 YP1YP2)
  (print-dispatch-test 21 TRUE TRUE [z] get-Z-slot 
     ZA1YA1XA1ZBYBXBXFYFZFXA2YA2ZA2 ZP1ZP2)
  (print-dispatch-test 22 TRUE FALSE [x] get-X-slot XA1XBXFXA2 XP1XP2)
  (print-dispatch-test 23 TRUE FALSE [x] get-Y-slot XA1XBXP1XP2XFXA2 "")
  (print-dispatch-test 24 TRUE FALSE [x] get-Z-slot XA1XBXP1XP2XFXA2 "")
  (print-dispatch-test 25 TRUE FALSE [y] get-X-slot 
     YA1XA1YBXBYP1YP2XFYFXA2YA2 "")
  (print-dispatch-test 26 TRUE FALSE [y] get-Y-slot 
     YA1XA1YBXBXFYFXA2YA2 YP1YP2)
  (print-dispatch-test 27 TRUE FALSE [y] get-Z-slot 
     YA1XA1YBXBYP1YP2XFYFXA2YA2 "")
  (print-dispatch-test 28 TRUE FALSE [z] get-X-slot 
     ZA1YA1XA1ZBYBXBZP1ZP2XFYFZFXA2YA2ZA2 "")
  (print-dispatch-test 29 TRUE FALSE [z] get-Y-slot 
     ZA1YA1XA1ZBYBXBZP1ZP2XFYFZFXA2YA2ZA2 "")
  (print-dispatch-test 30 TRUE FALSE [z] get-Z-slot 
     ZA1YA1XA1ZBYBXBXFYFZFXA2YA2ZA2 ZP1ZP2)
  (print-dispatch-test 31 FALSE TRUE [x] get-X-slot XA1XA2 "")
  (print-dispatch-test 32 FALSE TRUE [x] get-Y-slot XA1XA2 "")
  (print-dispatch-test 33 FALSE TRUE [x] get-Z-slot XA1XA2 "")
  (print-dispatch-test 34 FALSE TRUE [y] get-X-slot YA1YA2 "")
  (print-dispatch-test 35 FALSE TRUE [y] get-Y-slot YA1YA2 "")
  (print-dispatch-test 36 FALSE TRUE [y] get-Z-slot YA1YA2 "")
  (print-dispatch-test 37 FALSE TRUE [z] get-X-slot ZA1ZA2 "")
  (print-dispatch-test 38 FALSE TRUE [z] get-Y-slot ZA1ZA2 "")
  (print-dispatch-test 39 FALSE TRUE [z] get-Z-slot ZA1ZA2 "")
  (print-dispatch-test 40 FALSE FALSE [x] get-X-slot XA1XA2 "")
  (print-dispatch-test 41 FALSE FALSE [x] get-Y-slot XA1XA2 "")
  (print-dispatch-test 42 FALSE FALSE [x] get-Z-slot XA1XA2 "")
  (print-dispatch-test 43 FALSE FALSE [y] get-X-slot YA1YA2 "")
  (print-dispatch-test 44 FALSE FALSE [y] get-Y-slot YA1YA2 "")
  (print-dispatch-test 45 FALSE FALSE [y] get-Z-slot YA1YA2 "")
  (print-dispatch-test 46 FALSE FALSE [z] get-X-slot ZA1ZA2 "")
  (print-dispatch-test 47 FALSE FALSE [z] get-Y-slot ZA1ZA2 "")
  (print-dispatch-test 48 FALSE FALSE [z] get-Z-slot ZA1ZA2 ""))

