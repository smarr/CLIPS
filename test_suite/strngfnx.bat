(clear)                            ; 10.4.1
(str-cat)                          ; 10.4.1
(str-cat (create$))                ; 10.4.1
(str-cat a)                        ; 10.4.1
(str-cat "this one")               ; 10.4.1
(str-cat 8.25)                     ; 10.4.1
(str-cat 19838)                    ; 10.4.1
(str-cat [this-instance])          ; 10.4.1
(str-cat a "b c" 8.9 " " 78 [x])   ; 10.4.1
(str-cat "" "" "" "")              ; 10.4.1
(str-cat "foo" bar)                ; 10.4.1
(sym-cat)                          ; 10.4.2
(sym-cat (create$))                ; 10.4.2
(sym-cat a)                        ; 10.4.2
(sym-cat "this one")               ; 10.4.2
(sym-cat 8.5)                      ; 10.4.2
(sym-cat 19838)                    ; 10.4.2
(sym-cat [this-instance])          ; 10.4.2
(sym-cat a "b c" 8.9 " " 78 [x])   ; 10.4.2
(sym-cat "" "" "" "")              ; 10.4.2
(sub-string)                       ; 10.4.3
(sub-string 1)                     ; 10.4.3
(sub-string 1 3)                   ; 10.4.3
(sub-string 1 2 "acd" 3)           ; 10.4.3
(sub-string a 2 "abc")             ; 10.4.3
(sub-string 2 a "de")              ; 10.4.3
(sub-string 1 2 (create$ a b))     ; 10.4.3
(sub-string 1.0 2 "abc")           ; 10.4.3
(sub-string 1 2.0 "abc")           ; 10.4.3
(sub-string 1 2.5 "abc")           ; 10.4.3
(sub-string 2 2 abc)               ; 10.4.3
(sub-string 1 3 "abc")             ; 10.4.3
(sub-string -1 3 "abc")            ; 10.4.3
(sub-string 1 4 "xyz")             ; 10.4.3
(sub-string 3 1 "abc")             ; 10.4.3
(sub-string 1 1 "xyz")             ; 10.4.3
(sub-string 2 2 "xyz")             ; 10.4.3
(sub-string 3 3 "xyz")             ; 10.4.3
(sub-string 0 0 "abc")             ; 10.4.3
(sub-string 4 4 "abc")             ; 10.4.3
(sub-string 2 3 "xyzq")            ; 10.4.3
(sub-string 5 7 "abc")             ; 10.4.3
(sub-string -1 0 "abc")            ; 10.4.3
(sub-string 3 8 "abcdefghijkl")    ; 10.4.3
(str-index)                        ; 10.4.4
(str-index "a")                    ; 10.4.4
(str-index "a" "b" 3)              ; 10.4.4
(str-index 1 "a")                  ; 10.4.4
(str-index "a" 7.3)                ; 10.4.4
(str-index "a" a)                  ; 10.4.4
(str-index a "a")                  ; 10.4.4
(str-index "" "")                  ; 10.4.4
(str-index "" "xy")                ; 10.4.4
(str-index "xy" "")                ; 10.4.4
(str-index "ab" "abcd")            ; 10.4.4
(str-index "ab" "cabd")            ; 10.4.4
(str-index "ab" "cdab")            ; 10.4.4
(str-index "b" "xyzq")             ; 10.4.4
(str-index "ab" "axbayaazbbq")     ; 10.4.4
(str-index [ab] [dabc])            ; 10.4.4
(str-index "def" "abcdefghi")      ; 10.4.4
(str-index "qwerty" "qwertypoiuyt")
(str-index "qwerty" "poiuytqwer")  ; 10.4.4
(eval)                             ; 10.4.5
(eval "cat" "dog")                 ; 10.4.5
(eval 7.8)                         ; 10.4.5
(eval 900)                         ; 10.4.5
(eval [abc])                       ; 10.4.5
(eval (create$ a x y))             ; 10.4.5
(eval cat)                         ; 10.4.5
(eval "dog")                       ; 10.4.5
(eval "7")                         ; 10.4.5
(eval "893.34")                    ; 10.4.5
(eval "[ab]")                      ; 10.4.5
(eval "(+ 3 (* 4 5))")             ; 10.4.5
(eval "(facts)")                   ; 10.4.5
(eval "?x")                        ; 10.4.5
(eval "(defrule foo =>)")          ; 10.4.5
(eval "(+ 3 4)")                   ; 10.4.5
(eval "(mv-append a b c)")         ; 10.4.5
(build)                            ; 10.4.6
(build "(defrule foo =>)" "dog")   ; 10.4.6
(build 7.8)                        ; 10.4.6
(build 900)                        ; 10.4.6
(build [abc])                      ; 10.4.6
(build (create$ a x y))            ; 10.4.6
(build deftemplate)                ; 10.4.6
(build "(defrule foo =>)")         ; 10.4.6
(build "(defrule blah)")           ; 10.4.6
(build "(+ 3 4)")                  ; 10.4.6
(build "defrule foo =>)")          ; 10.4.6
(rules)                            ; 10.4.6
(clear)                            ; 10.4.6
(upcase)                           ; 10.4.7
(upcase "abc" "def")               ; 10.4.7
(upcase abc)                       ; 10.4.7
(upcase "xyz")                     ; 10.4.7
(upcase 8993.34)                   ; 10.4.7
(upcase 8993)                      ; 10.4.7
(upcase [another-thingo])          ; 10.4.7
(upcase (create$ jkkj 78 3.45))    ; 10.4.7
(upcase "this is a sentence")      ; 10.4.7
(upcase "THIS IS A SENTENCE")      ; 10.4.7
(upcase "aA1234567890zZ")          ; 10.4.7
(upcase "aA!@#$%^&*()zZ")          ; 10.4.7
(upcase "`~-_=+[{]}|;:',<.>?")     ; 10.4.7
(upcase "This is a test of upcase")
(upcase A_Word_Test_for_Upcase)    ; 10.4.7
(lowcase)                          ; 10.4.8
(lowcase "ABC" "DEF")              ; 10.4.8
(lowcase ABC)                      ; 10.4.8
(lowcase "XYZ")                    ; 10.4.8
(lowcase 8993.34)                  ; 10.4.8
(lowcase 8993)                     ; 10.4.8
(lowcase [another-thingo])         ; 10.4.8
(lowcase (create$ jkkj 78 3.45))   ; 10.4.8
(lowcase "this is a sentence")     ; 10.4.8
(lowcase "THIS IS A SENTENCE")     ; 10.4.8
(lowcase "aA1234567890zZ")         ; 10.4.8
(lowcase "aA!@#$%^&*()zZ")         ; 10.4.8
(lowcase "`~-_=+[{]}|;:',<.>?")    ; 10.4.8
(lowcase "this is a test of lowcase")
(lowcase A_Word_Test_for_Lowcase)  ; 10.4.8
(str-compare)                      ; 10.4.9
(str-compare "xyz")                ; 10.4.9
(str-compare "zykk" "kkdfj" "df")  ; 10.4.9
(str-compare 783.9 "dlfj")         ; 10.4.9
(str-compare [xyz] "ldf")          ; 10.4.9
(str-compare kkdm 9883)            ; 10.4.9
(str-compare klkll (create$ a))    ; 10.4.9
(str-compare abc ABC)              ; 10.4.9
(str-compare ABC abc)              ; 10.4.9
(str-compare "abcd" "abce")        ; 10.4.9
(str-compare "ABCD" ABCE)          ; 10.4.9
(str-compare abc xyz)              ; 10.4.9
(str-compare xyz abc)              ; 10.4.9
(str-compare [a23] [a24])          ; 10.4.9
(str-compare same "same")          ; 10.4.9
(str-compare "string1" "string2")  ; 10.4.9
(str-length)                       ; 10.4.10
(str-length "zykk" "kkdfj")        ; 10.4.10
(str-length 783.9)                 ; 10.4.10
(str-length [xyz])                 ; 10.4.10
(str-length 9883)                  ; 10.4.10
(str-length (create$ a))           ; 10.4.10
(str-length "")                    ; 10.4.10
(str-length "x y z")               ; 10.4.10
(str-length a)                     ; 10.4.10
(str-length "abcd")                ; 10.4.10
(str-length xyz)                   ; 10.4.10
(str-length "Привет мир")          ; UTF-8 Support
(str-length "여러분 안녕하세요")       ; UTF-8 Support
(str-length "Olá Mundo")           ; UTF-8 Support
