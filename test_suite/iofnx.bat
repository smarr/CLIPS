(clear)                            
(open)                             ; 10.5.2.1
(open "blah1.dat")                 ; 10.5.2.1
(open "blah2.dat" blah2 "r" 10)    ; 10.5.2.1
(open 10 blah3 "r")                ; 10.5.2.1
(open [blah2.dat] blah4 "r")       ; 10.5.2.1
(open "blah4.dat" (create$) "r")   ; 10.5.2.1
(open blah5.dat blah5 r)           ; 10.5.2.1
(open blah6.dat blah6 "x")         ; 10.5.2.1
(open blah7.dat blah7 8)           ; 10.5.2.1
(open blah8.dat blah8 "r")         ; 10.5.2.1
(close blah6 89)                   ; 10.5.2.2
(close (create$))                  ; 10.5.2.2
(close [blah8])                    ; 10.5.2.2
(printout)                         ; 10.5.2.3
(printout (create$))               ; 10.5.2.3
(read (create$))                   ; 10.5.2.4
(read bogus)                       ; 10.5.2.4
(read stdin stdout)                ; 10.5.2.4
(read)                             ; 10.5.2.4 - 7
7
(read t)                           ; 10.5.2.4 - abc
abc
(read stdin)                       ; 10.5.2.4 - xyz
xyz abc
(read)                             ; 10.5.2.4 - a


    a    
(open "Temp//iofnx1.tmp" mydata "w")     ; 10.5.2.4
(printout mydata "red green")      ; 10.5.2.4
(close mydata)                     ; 10.5.2.4
(open "Temp//iofnx1.tmp" mydata)   ; 10.5.2.4
(read mydata)                      ; 10.5.2.4
(read mydata)                      ; 10.5.2.4
(read mydata)                      ; 10.5.2.4
(close mydata)                     ; 10.5.2.4
(readline (create$))               ; 10.5.2.5
(readline bogus)                   ; 10.5.2.5
(readline stdin stdout)            ; 10.5.2.5
(readline)                         ; 10.5.2.5 - "7"
7
(readline t)                       ; 10.5.2.5 - "abc"
abc
(readline stdin)                   ; 10.5.2.5 - "xyz abc"
xyz abc
(readline)                         ; 10.5.2.5 - ""

(open "Temp//iofnx1.tmp" 7.8923)   ; 10.5.2.5
(readline 7.8923)                  ; 10.5.2.5
(readline 7.8923)                  ; 10.5.2.5
(close 7.8923)                     ; 10.5.2.5
(format)                           ; 10.5.2.6
(format t)                         ; 10.5.2.6
(format (create$))                 ; 10.5.2.6
(format t "%f%%%n")                ; 10.5.2.6
(format nil "Integer: |%d|" 12)    ; 10.5.2.6
(format t "Integer: |%4d|" 12)     ; 10.5.2.6
(format nil "Integer: |%-04d|" 12)
(format t "Float:   |%f|" 12.01)   ; 10.5.2.6
(format nil "Float:   |%7.2f| "12.01)
(format t "Test:    |%e|" 12.01)   ; 10.5.2.6
(format nil "Test:    |%7.2e|" 12.01)
(format t "General: |%g|" 1234567890)
(format t "Hexadecimal: |%x|" 12)  ; 10.5.2.6
(format t "Octal:   |%o|" 12)      ; 10.5.2.6
(format nil "Symbols: |%s| |%s|" value-a1 capacity)
(format nil "the %% x %d x %s x %f y %%" 4 ab 3.5)
(format nil "%d" abc)              ; 10.5.2.6
(format nil "%d" 9.8)              ; 10.5.2.6
(format t "%f" 40)                 ; 10.5.2.6
(format nil "%f" abc)              ; 10.5.2.6
(format nil "%g" (create$))        ; 10.5.2.6
(format t "%o" 9.8)                ; 10.5.2.6
