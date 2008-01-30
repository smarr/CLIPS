(clear)                            ; 10.3.1
(create$)                        ; 10.3.1
(create$ a "a" 3 5.1 [x])          ; 10.3.1
(create$ (create$) (create$))  ; 10.3.1
(create$ (create$ a b) (create$ c)); 10.3.1
(create$ a (create$ e f))          ; 10.3.1
(create$ (create$ d x y) a 2)      ; 10.3.1
(create$ hammer drill saw screw pliers wrench)
(create$ (+ 3 4) (* 2 3) (/ 8 4))
(nth$)                              ; 10.3.2
(nth$ 1)                           ; 10.3.2
(nth$ 2 (create$ a b c) 5)          ; 10.3.2
(nth$ a (create$ x y z))           ; 10.3.2
(nth$ 1 x)                         ; 10.3.2
(nth$ 2 (create$ d e f))         ; 10.3.2
(nth$ -1 (create$ x y z))          ; 10.3.2
(nth$ 0 (create$ x y z))           ; 10.3.2
(nth$ 4 (create$ x y z))            ; 10.3.2
(nth$ 2 (create$ x y z))            ; 10.3.2
(nth$ -1 (create$))                ; 10.3.2
(nth$ 0 (create$))                 ; 10.3.2
(nth$ 1 (create$))                 ; 10.3.2
(nth$ 2 (create$))                 ; 10.3.2
(nth$ 3 (create$ a b c d e f g))  ; 10.3.2
(member$)                           ; 10.3.3
(member$ x)                        ; 10.3.3
(member$ x (create$ y x q) r)       ; 10.3.3
(member$ a (create$ 3 4.1 "a" [a] a))
(member$ "a" (create$ 3 a 4.1 [a] "a"))
(member$ [a] (create$ 3 4.1 "a" a [a]))
(member$ 3 (create$ 4.1 3.0 "a" [a] 3 a))
(member$ 4 (create$ 4 a 4.0 "a" [a]))
(member$ (create$ a) (create$ b a))
(member$ a (create$ a b c))        ; 10.3.3
(member$ x (create$ a b c))        ; 10.3.3
(member$ blue (create$ red 3 "text" 8.7 blue))
(member$ 4 (create$ red 3 "text" 8.7 blue))
(subsetp)                          ; 10.3.4
(subsetp (create$ a))              ; 10.3.4
(subsetp (create$ a) (create$ b) 3)
(subsetp 1 (create$ a))            ; 10.3.4
(subsetp (create$ a) a)            ; 10.3.4
(subsetp (create$) (create$))      ; 10.3.4
(subsetp (create$) (create$ x y))  ; 10.3.4
(subsetp (create$ x y) (create$))  ; 10.3.4
(subsetp (create$ a b) (create$ b a))
(subsetp (create$ a) (create$ a b))
(subsetp (create$ b) (create$ a b))
(subsetp (create$ b a a b) (create$ c a b d))
(subsetp (create$ a b c) (create$ a b d))
(subsetp (create$ a) (create$ 3 4.1 "a" [a])) 
(subsetp (create$ [a]) (create$ 3 4.1 "a" a))  
(subsetp (create$ "a") (create$ 3 4.1 a [a]))  
(subsetp (create$ 3) (create$ 3.0 "a" [a]))   
(subsetp (create$ 3.0) (create$ 3 "a" [a])) 
(subsetp (create$ a) (create$ 3 a 4.1)) 
(subsetp (create$ [a]) (create$ [a] 3 4.1))  
(subsetp (create$ "a") (create$ 3 4.1 "a"))  
(subsetp (create$ 3) (create$ 3 "a" [a]))   
(subsetp (create$ 3.0) (create$ "a" [a] 3.0)) 
(subsetp (create$ hammer saw drill)
         (create$ hammer drill wrench pliers saw))
(subsetp (create$ wrench crowbar)
         (create$ hammer drill wrench pliers saw))  
(delete$)                        ; 10.3.5
(delete$ 1)                        ; 10.3.5
(delete$ (create$ a b c) [x] 3 3)    ; 10.3.5
(delete$ (create$ a b c d) 3.4 3.4)    ; 10.3.5
(delete$ (create$ a b c d) a a)      ; 10.3.5
(delete$ "a b" 3 3)                ; 10.3.5
(delete$ (create$ x 1 z) -1 -1)     ; 10.3.5
(delete$ (create$ x 4.2 z) 0 0)    ; 10.3.5
(delete$ (create$ [ab] cd ef) 1 1) ; 10.3.5
(delete$ (create$ abc d ef) 2 2)   ; 10.3.5
(delete$ (create$ abcd 2 e) 3 3)   ; 10.3.5
(delete$ (create$ q 3 "tx") 4 4)   ; 10.3.5
(delete$ (create$) -1 -1)             ; 10.3.5
(delete$ (create$) 0 0)              ; 10.3.5
(delete$ (create$) 1 1)              ; 10.3.5
(delete$ (create$) 2 2)              ; 10.3.5
(delete$ (create$ hammer drill saw pliers wrench) 3 3)
(delete$ (create$ computer printer hard-disk) 1 1)
(delete$ (create$ computer printer hard-disk floppy-drive) 2 3)
(delete$ (create$ computer printer hard-disk floppy-drive) 1 4)
(delete$ (create$ computer printer hard-disk floppy-drive) 1 2)
(delete$ (create$ computer printer hard-disk floppy-drive) 1 3)
(delete$ (create$ computer printer hard-disk floppy-drive) 2 4)
(delete$ (create$ computer printer hard-disk floppy-drive) 3 4)
(delete$ (create$ computer printer hard-disk floppy-drive) 3 2)
(length)                           ; 10.3.6
(length "x" 3)                     ; 10.3.6
(length [def])                     ; 10.3.6
(length 4.3)                       ; 10.3.6
(length 567)                       ; 10.3.6
(length blah-blah-blah)            ; 10.3.6
(length "")                        ; 10.3.6
(length "Hello world")             ; 10.3.6
(length (create$))                 ; 10.3.6
(length (create$ a b c))           ; 10.3.6
(length (create$ a b c d e f g)) ; 10.3.6
(length "cat")                     ; 10.3.6
(explode$)                      ; 10.3.7
(explode$ "x" 3)                ; 10.3.7
(explode$ [jocko])              ; 10.3.7
(explode$ 134.78)               ; 10.3.7
(explode$ -8939)                ; 10.3.7
(explode$ yak-yak)              ; 10.3.7
(explode$ "")                   ; 10.3.7
(explode$ "Hello world")        ; 10.3.7
(explode$ "-3.45 89 [xyx] thishere \"a string\"")
(explode$ "hammer drill saw screw")
(explode$ "1 2 abc 3 4 \"abc\" \"def\"")
(explode$ "?x ~ )")             ; 10.3.7
(implode$)                      ; 10.3.8
(implode$ (create$ a b) dkj)    ; 10.3.8
(implode$ [outstanding])        ; 10.3.8
(implode$ 3499.3)               ; 10.3.8
(implode$ 203033)               ; 10.3.8
(implode$ moresymbols)          ; 10.3.8
(implode$ "Another string")     ; 10.3.8
(implode$ (create$))            ; 10.3.8
(implode$ (create$ axydk "here it is" -348e3 893 [instance]))
(implode$ (create$ hammer drill screwdriver))
(implode$ (create$ 1 "abc" def "ghi" 2))
(implode$ (explode$ "abc      def     ghi"))
(subseq$)                        ; 10.3.9
(subseq$ 1)                      ; 10.3.9
(subseq$ 1 3)                    ; 10.3.9
(subseq$ (create$ a c d) 1 2 3)  ; 10.3.9
(subseq$ (create$ a b c) a 2)    ; 10.3.9
(subseq$ (create$ d e) 2 a)      ; 10.3.9
(subseq$ this 1 2)               ; 10.3.9
(subseq$ (create$ a b c) 1 2)  ; 10.3.9
(subseq$ (create$ a b c) 1 2)  ; 10.3.9
(subseq$ (create$ a b c) 1 2.5)  ; 10.3.9
(subseq$ (create$ a b c) 1 3)    ; 10.3.9
(subseq$ (create$ a b c) -1 3)   ; 10.3.9
(subseq$ (create$ a "b" c) 1 4)  ; 10.3.9
(subseq$ (create$ a b c) 3 1)    ; 10.3.9
(subseq$ (create$ 3 b c) 1 1)    ; 10.3.9
(subseq$ (create$ a 4.1 c) 2 2)  ; 10.3.9
(subseq$ (create$ a b [x]) 3 3)  ; 10.3.9
(subseq$ (create$ a b c) 0 0)    ; 10.3.9
(subseq$ (create$ a b c) 4 4)    ; 10.3.9
(subseq$ (create$ a b c d) 2 3)  ; 10.3.9
(subseq$ (create$ a b c) 5 7)    ; 10.3.9
(subseq$ (create$ a b c) -1 0)   ; 10.3.9
(subseq$ (create$ hammer drill wrench pliers) 3 4)
(subseq$ (create$ a "abc" def "ghi" 2)  1 1)
(replace$)                       ; 10.3.10
(replace$ 1)                     ; 10.3.10
(replace$ (create$ a b) 1 1)       ; 10.3.10
(replace$ (create$ a b) 1 1 3 4)   ; 10.3.10
(replace$ (create$ a) 1.5 1.5 [x])   ; 10.3.10
(replace$ 3.5 1 1 a)               ; 10.3.10
(replace$ (create$ a b) 2 2(create$))
(replace$ (create$ a b) -1 -1 x)    ; 10.3.10
(replace$ (create$ c d) 0 0 3.5)   ; 10.3.10
(replace$ (create$ "c" "d") 1 1 13)
(replace$ (create$ 3 5) 2 2 "x")   ; 10.3.10
(replace$ (create$ [a] [b]) 3 3 3) ; 10.3.10
(replace$ (create$ drill wrench pliers) 3 3 machete)
(replace$ (create$ drill wrench pliers hammer) 2 3 machete)
(replace$ (create$ drill wrench pliers hammer) 2 4 machete knife)
(replace$ (create$ drill wrench pliers hammer) 1 4 (create$))
(replace$ (create$ drill wrench pliers hammer) 1 2 machete (create$))
(replace$ (create$ drill wrench pliers hammer) 3 4 machete)
(replace$ (create$ drill wrench pliers hammer) 4 3 machete)
(insert$ (create$ drill wrench pliers hammer) 1 machete)
(insert$ (create$ drill wrench pliers hammer) 2 machete)
(insert$ (create$ drill wrench pliers hammer) 3 machete)
(insert$ (create$ drill wrench pliers hammer) 4 machete)
(insert$ (create$ drill wrench pliers hammer) 3 machete knife)
(insert$ (create$ drill wrench pliers hammer) 2 machete knife (create$))
(insert$ (create$ drill wrench pliers hammer) 5 machete)
(progn$)
(progn$ abc)
(progn$ (create$) (bind ?field))
(progn$ (?field (create$)) (bind ?field))
(progn (bind ?x 0) 
   (progn$ (create$ 1 2 3) (bind ?x (+ ?x ?field))))
(progn (bind ?x 0) 
   (progn$ (?field (create$ 1 2 3)) (bind ?x (+ ?x ?field))))
(progn
  (bind ?outer (create$ abc def ghi jkl mno))
  (progn$ (?x ?outer)
    (bind ?inner (subseq$ ?outer ?x-index (length$ ?outer)))
    (progn$ (?y ?inner)
       (printout t ?y " "))
    (printout t crlf)))
(foreach)
(foreach abc)
(foreach (create$) (bind ?field))
(foreach ?field (create$) (bind ?field))
(progn (bind ?x 0) 
   (foreach ?field (create$ 1 2 3) (bind ?x (+ ?x ?field))))
(progn
  (bind ?outer (create$ abc def ghi jkl mno))
  (foreach ?x ?outer
    (bind ?inner (subseq$ ?outer ?x-index (length$ ?outer)))
    (foreach ?y ?inner
       (printout t ?y " "))
    (printout t crlf)))
(first$)
(first$ 1)
(first$ (create$ a b c) a)
(first$ (create$ a b c))
(first$ (create$ a))
(first$ (create$))
(rest$)
(rest$ 1)
(rest$ (create$ a b c) a)
(rest$ (create$ a b c))
(rest$ (create$ a))
(rest$ (create$))
(mv-delete)                        
(mv-delete 1)                        
(mv-delete 3 (create$ a b c) [x])    
(mv-delete 3.4 (create$ a b c d))    
(mv-delete a (create$ a b c d))      
(mv-delete 3 "a b")                
(mv-delete -1 (create$ x 1 z))     
(mv-delete 0 (create$ x 4.2 z))    
(mv-delete 1 (create$ [ab] cd ef)) 
(mv-delete 2 (create$ abc d ef))   
(mv-delete 3 (create$ abcd 2 e))   
(mv-delete 4 (create$ q 3 "tx"))   
(mv-delete -1 (create$))             
(mv-delete 0 (create$))              
(mv-delete 1 (create$))              
(mv-delete 2 (create$))              
(mv-delete 3 (create$ hammer drill saw pliers wrench))
(mv-delete 1 (create$ computer printer hard-disk))
(mv-subseq)                        
(mv-subseq 1)                      
(mv-subseq 1 3)                    
(mv-subseq 1 2 (create$ a c d) 3)  
(mv-subseq a 2 (create$ a b c))    
(mv-subseq 2 a (create$ d e))      
(mv-subseq 1 2 this)               
(mv-subseq 1 2 (create$ a b c))  
(mv-subseq 1 2 (create$ a b c))  
(mv-subseq 1 2.5 (create$ a b c))  
(mv-subseq 1 3 (create$ a b c))    
(mv-subseq -1 3 (create$ a b c))   
(mv-subseq 1 4 (create$ a "b" c))  
(mv-subseq 3 1 (create$ a b c))    
(mv-subseq 1 1 (create$ 3 b c))    
(mv-subseq 2 2 (create$ a 4.1 c))  
(mv-subseq 3 3 (create$ a b [x]))  
(mv-subseq 0 0 (create$ a b c))    
(mv-subseq 4 4 (create$ a b c))    
(mv-subseq 2 3 (create$ a b c d))  
(mv-subseq 5 7 (create$ a b c))    
(mv-subseq -1 0 (create$ a b c))   
(mv-subseq 3 4 (create$ hammer drill wrench pliers))
(mv-subseq 1 1 (create$ a "abc" def "ghi" 2))
(mv-replace)                       
(mv-replace 1)                     
(mv-replace 1 (create$ a b))       
(mv-replace 1 (create$ a b) 3 4)   
(mv-replace 1.5 (create$ a) [x])   
(mv-replace 1 3.5 a)               
(mv-replace 2 (create$ a b) (create$))
(mv-replace -1 (create$ a b) x)    
(mv-replace 0 (create$ c d) 3.5)   
(mv-replace 1 (create$ "c" "d") 13)
(mv-replace 2 (create$ 3 5) "x")   
(mv-replace 3 (create$ [a] [b]) 3) 
(mv-replace 3 (create$ drill wrench pliers) machete)
