(defrule hello
   =>
   (printout t "Hello World." crlf)
   (printout t "Hit return to end." crlf)
   (readline)
   ;(exit) ; Need DLL to free library
   )
   