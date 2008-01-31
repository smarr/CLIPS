(defrule error-1 "constant & $?var"
  (bar $?y&yak)
  =>)
(defrule error-2 "constant & $?var"
  (bar $?y yak&$?y)
  =>)
(defrule error-3 "constant & $?var"
  (bar yak&$?y)
  =>)
(defrule error-4 "constant & $?var"
  (bar $?y $?y|yak)
  =>)
(defrule error-5 "constant & $?var"
  (bar $?y yak|$?y)
  =>)
(defrule error-6 "constant & $?var"
  (bar yak|$?y)
  =>)
  
(defrule error-7 "? return value & $?var"
  (bar $?y&=(+ 3 4))
  =>)
(defrule error-8 "? return value & $?var"
  (bar $?y =(+ 3 4)&$?y)
  =>)
(defrule error-9 "? return value & $?var"
  (bar =(+ 3 4)&$?y)
  =>)
(defrule error-10 "? return value & $?var"
  (bar $?y $?y|=(+ 3 4))
  =>)
(defrule error-11 "? return value & $?var"
  (bar $?y =(+ 3 4)|$?y)
  =>)
(defrule error-12 "? return value & $?var"
  (bar =(+ 3 4)|$?y)
  =>)
  
(defrule okay!-1 "$? return value & $?var"
  (bar $?y&=(create$))
  =>)
(defrule okay!-2 "$? return value & $?var"
  (bar $?y =(create$)&$?y)
  =>)
(defrule error-12a "$? return value & $?var"
  (bar =(create$)&$?y)
  =>)
(defrule okay!-3 "$? return value & $?var"
  (bar $?y $?y|=(create$))
  =>)
(defrule okay!-4 "$? return value & $?var"
  (bar $?y =(create$)|$?y)
  =>)
(defrule error-12b "$? return value & $?var"
  (bar =(create$)|$?y)
  =>)
 
(defrule error-13 "$?var & $?var"
  (bar  $?z&$?y)
  =>)
(defrule okay!-5 "$?var & $?var"
  (bar $?y $?z&$?y)
  =>)
(defrule error-14 "$?var & $?var"
  (bar $?z $?z&$?y)
  =>)
(defrule okay!-6 "$?var & $?var"
  (bar $?z $?y $?z&$?y)
  =>)
(defrule error-15 "$?var & $?var"
  (bar $?z|$?y)
  =>)
(defrule error-16 "$?var & $?var"
  (bar $?y $?z|$?y)
  =>)
(defrule error-17 "$?var & $?var"
  (bar $?z $?z|$?y)
  =>)
(defrule okay!-7 "$?var & $?var"
  (bar $?z $?y $?z|$?y)
  =>)

(defrule error-18 "?var & ?var"
  (bar ?z&?y)
  =>)
(defrule okay!-8 "?var & ?var"
  (bar ?y ?z&?y)
  =>)
(defrule error-19 "?var & ?var"
  (bar ?z ?z&?y)
  =>)
(defrule okay!-9 "?var & ?var"
  (bar ?z ?y ?z&?y)
  =>)
(defrule error-20 "?var & ?var"
  (bar ?z|?y)
  =>)
(defrule error-21 "?var & ?var"
  (bar ?y ?z|?y)
  =>)
(defrule error-22 "?var & ?var"
  (bar ?z ?z|?y)
  =>)
(defrule okay!-10 "?var & ?var"
  (bar ?z ?y ?z|?y)
  =>)

(defrule error-23 "$?var & ?var"
  (bar $?z&?y)
  =>)
(defrule error-24 "$?var & ?var"
  (bar ?y $?z&?y)
  =>)
(defrule error-25 "$?var & ?var"
  (bar $?z $?z&?y)
  =>)
(defrule error-26 "$?var & ?var"
  (bar $?z ?y $?z&?y)
  =>)
(defrule error-27 "$?var & ?var"
  (bar $?z|?y)
  =>)
(defrule error-28 "$?var & ?var"
  (bar ?y $?z|?y)
  =>)
(defrule error-29 "$?var & ?var"
  (bar $?z $?z|?y)
  =>)
(defrule error-30 "$?var & ?var"
  (bar $?z ?y $?z|?y)
  =>)
(defrule error-31 "$?var & ?var"
  (bar ?z&$?y)
  =>)
(defrule error-32 "$?var & ?var"
  (bar $?y ?z&$?y)
  =>)
(defrule error-33 "$?var & ?var"
  (bar ?z ?z&$?y)
  =>)
(defrule error-34 "$?var & ?var"
  (bar ?z $?y ?z&$?y)
  =>)
(defrule error-35 "$?var & ?var"
  (bar ?z|$?y)
  =>)
(defrule error-36 "$?var & ?var"
  (bar $?y ?z|$?y)
  =>)
(defrule error-37 "$?var & ?var"
  (bar ?z ?z|$?y)
  =>)
(defrule error-38 "$?var & ?var"
  (bar ?z $?y ?z|$?y)
  =>)
(defrule error-39 "$?var & ?var"
  (bar $?y ?z&?y)
  =>)
(defrule error-40 "$?var & ?var"
  (bar ?y ?z&$?y)
  =>)
(defrule error-41 "$?var & ?var"
  (bar ?z $?y ?z&?y)
  =>)  
(defrule error-42 "$?var & ?var"
  (bar ?z ?y ?z&$?y)
  =>) 
(defrule error-43 "$?var & ?var"
  (bar $?y ?z|?y)
  =>)
(defrule error-44 "$?var & ?var"
  (bar ?y ?z|$?y)
  =>)  
(defrule error-45 "$?var & ?var"
  (bar ?z $?y ?z|?y)
  =>)
(defrule error-46 "$?var & ?var"
  (bar ?z ?y ?z|$?y)
  =>)
(defrule error-47 "$?var & ?var"
  (bar $?z ?z&?y)
  =>)
(defrule error-48 "$?var & ?var"
  (bar ?z $?z&?y)
  =>)
(defrule error-49 "$?var & ?var"
  (bar $?z ?y ?z&?y)
  =>)
(defrule error-50 "$?var & ?var"
  (bar ?z ?y $?z&?y)
  =>)
(defrule error-51 "$?var & ?var"
  (bar $?z ?z|?y)
  =>)
(defrule error-52 "$?var & ?var"
  (bar ?z $?z|?y)
  =>)
(defrule error-53 "$?var & ?var"
  (bar $?z ?y ?z|?y)
  =>)   
(defrule error-54 "$?var & ?var"
  (bar ?z ?y $?z|?y)
  =>)    
  
(defrule okay!-11 "constant & ?var"
  (bar ?y&yak)
  =>)
(defrule okay!-12 "constant & ?var"
  (bar ?y yak&?y)
  =>)
(defrule error-54a "constant & ?var"
  (bar yak&?y)
  =>)
(defrule okay!-13 "constant & ?var"
  (bar ?y ?y|yak)
  =>)
(defrule okay!-14 "constant & ?var"
  (bar ?y yak|?y)
  =>)
(defrule error-54b "constant & ?var"
  (bar yak|?y)
  =>)

(defrule okay!-15 "? return value & ?var"
  (bar ?y&=(+ 3 4))
  =>)
(defrule okay!-16 "? return value & ?var"
  (bar ?y =(+ 3 4)&?y)
  =>)
(defrule error-55 "? return value & ?var"
  (bar =(+ 3 4)&?y)
  =>)
(defrule okay!-17 "? return value & ?var"
  (bar ?y ?y|=(+ 3 4))
  =>)
(defrule okay!-18 "? return value & ?var"
  (bar ?y =(+ 3 4)|?y)
  =>)
(defrule error-56 "? return value & ?var"
  (bar =(+ 3 4)|?y)
  =>)

(defrule error-57 "$? return value & ?var"
  (bar ?y&=(create$))
  =>)
(defrule error-58 "$? return value & ?var"
  (bar ?y =(create$)&?y)
  =>)
(defrule error-59 "$? return value & ?var"
  (bar =(create$)&?y)
  =>)
(defrule error-60 "$? return value & ?var"
  (bar ?y ?y|=(create$))
  =>)
(defrule error-61 "$? return value & ?var"
  (bar ?y =(create$)|?y)
  =>)
(defrule error-62 "$? return value & ?var"
  (bar =(create$)|?y)
  =>)

(defrule okay!-19 "constant & constant"
  (bar yak&yak)  
  =>)
(defrule okay!-20 "constant & constant"
  (bar yak|yak)
  =>)  
  
(defrule okay!-21 "? return value & ? return value"
  (bar =(+ 3 4)&=(+ 3 4))
  =>)
(defrule okay!-22 "? return value & ? return value"
  (bar =(+ 3 4)|=(+ 3 4))
  =>)
  
(defrule okay!-23 "$? return value & $? return value"
  (bar =(create$)&=(create$))
  =>)
(defrule okay!-24 "$? return value & $? return value"
  (bar =(create$)|=(create$))
  =>)
  
(defrule error-63 "? return value & $? return value"
  (bar =(+ 3 4)&=(create$))
  =>)
(defrule error-64 "? return value & $? return value"
  (bar =(create$)&=(+ 3 4))
  =>)  
(defrule error-65 "? return value & $? return value"
  (bar =(+ 3 4)|=(create$))
  =>)
(defrule error-66 "? return value & $? return value"
  (bar =(create$)|=(+ 3 4))
  =>)
  
(defrule okay!-25 "constant & ? return value"
  (bar 7&=(+ 3 4))
  =>)
(defrule okay!-26 "constant & ? return value"
  (bar =(+ 3 4)&7)
  =>)  
(defrule okay!-27 "constant & ? return value"
  (bar yak|=(+ 3 4))
  =>) 
(defrule okay!-28 "constant & ? return value"
  (bar =(+ 3 4)|yak)
  =>)
  
(defrule error-67 "constant & $? return value"
  (bar 7&=(create$))  
(defrule error-68 "constant & $? return value"
  (bar =(create$)&7)
  =>)  
(defrule error-69 "constant & $? return value"
  (bar yak|=(create$)) 
(defrule error-70 "constant & $? return value"
  (bar =(create$)|yak)
  =>)

(defrule error-71 "? and $? mixing"
  (foo ?f $?f)
  =>)
(defrule error-72 "? and $? mixing"
  (foo $?f ?f)
  =>)
(defrule error-73 "? and $? mixing"
  ?f <- (foo $?f)
  =>)
(defrule error-74 "? and $? mixing"
  $?f <- (foo ?f)
  =>)
(defrule error-75 "? and $? mixing"
  (foo ?f)
  (foo $?f)
  =>)
(defrule error-76 "? and $? mixing"
  (foo $?f)
  (foo ?f)
  =>)
(defrule error-77 "? and $? mixing"
  ?f <- (foo)
  (foo $?f)
  =>)
(defrule error-78 "? and $? mixing"
  $?f <- (foo)
  (foo ?f)
  =>)
(defrule error-79 "? and $? mixing"
  (foo ?f $?x&$?f)
  =>)
(defrule error-80 "? and $? mixing"
  (foo ?f $?x|$?f)
  =>)
(defrule error-81 "? and $? mixing"
  (foo $?f ?x&?f)
  =>)
(defrule error-82 "? and $? mixing"
  (foo $?f ?x|?f)
  =>)
(defrule error-83 "? and $? mixing"
  (foo $?f)
  (foo ?x&?f)
  =>)
(defrule error-84 "? and $? mixing"
  (foo ?f)
  (foo $?x|$?f)
  =>)