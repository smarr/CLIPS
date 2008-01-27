
;;;======================================================
;;;   Who Drinks Water? And Who owns the Zebra?
;;;     
;;;     Another puzzle problem in which there are five 
;;;     houses, each of a different color, inhabited by 
;;;     men of different nationalities, with different 
;;;     pets, drinks, and cigarettes. Given the initial
;;;     set of conditions, it must be determined which
;;;     attributes are assigned to each man.
;;;
;;;     CLIPS Version 6.0 Example
;;;
;;;     To execute, merely load, reset and run.
;;;======================================================
    
(deftemplate avh (field a) (field v) (field h))

(defrule find-solution
  ; The Englishman lives in the red house.

  (avh (a nationality) (v englishman) (h ?n1))
  (avh (a color) (v red) (h ?c1&?n1))

  ; The Spaniard owns the dog.

  (avh (a nationality) (v spaniard) (h ?n2&~?n1))
  (avh (a pet) (v dog) (h ?p1&?n2))

  ; The ivory house is immediately to the left of the green house,
  ; where the coffee drinker lives.

  (avh (a color) (v ivory) (h ?c2&~?c1))
  (avh (a color) (v green) (h ?c3&~?c2&~?c1&=(+ ?c2 1)))
  (avh (a drink) (v coffee) (h ?d1&?c3))

  ; The milk drinker lives in the middle house.

  (avh (a drink) (v milk) (h ?d2&~?d1&3))

  ; The man who smokes Old Golds also keeps snails.

  (avh (a smokes) (v old-golds) (h ?s1))
  (avh (a pet) (v snails) (h ?p2&~?p1&?s1))

  ; The Ukrainian drinks tea.

  (avh (a nationality) (v ukrainian) (h ?n3&~?n2&~?n1))
  (avh (a drink) (v tea) (h ?d3&~?d2&~?d1&?n3))

  ; The Norwegian resides in the first house on the left.

  (avh (a nationality) (v norwegian) (h ?n4&~?n3&~?n2&~?n1&1))

  ; Chesterfields smoker lives next door to the fox owner.

  (avh (a smokes) (v chesterfields) (h ?s2&~?s1))
  (avh (a pet) (v fox) (h ?p3&~?p2&~?p1&:(or (= ?s2 (- ?p3 1)) (= ?s2 (+ ?p3 1)))))

  ; The Lucky Strike smoker drinks orange juice.

  (avh (a smokes) (v lucky-strikes) (h ?s3&~?s2&~?s1))
  (avh (a drink) (v orange-juice) (h ?d4&~?d3&~?d2&~?d1&?s3)) 

  ; The Japanese smokes Parliaments

  (avh (a nationality) (v japanese) (h ?n5&~?n4&~?n3&~?n2&~?n1))
  (avh (a smokes) (v parliaments) (h ?s4&~?s3&~?s2&~?s1&?n5))

  ; The horse owner lives next to the Kools smoker, 
  ; whose house is yellow.

  (avh (a pet) (v horse) (h ?p4&~?p3&~?p2&~?p1))
  (avh (a smokes) (v kools) (h ?s5&~?s4&~?s3&~?s2&~?s1&:(or (= ?p4 (- ?s5 1)) (= ?p4 (+ ?s5 1)))))
  (avh (a color) (v yellow) (h ?c4&~?c3&~?c2&~?c1&?s5))

  ; The Norwegian lives next to the blue house.

  (avh (a color) (v blue) (h ?c5&~?c4&~?c3&~?c2&~?c1&:(or (= ?c5 (- ?n4 1)) (= ?c5 (+ ?n4 1)))))
  
  ; Who drinks water?  And Who owns the zebra?

  (avh (a drink) (v water) (h ?d5&~?d4&~?d3&~?d2&~?d1))
  (avh (a pet) (v zebra) (h ?p5&~?p4&~?p3&~?p2&~?p1))

  => 
  (assert (solution nationality englishman ?n1)
          (solution color red ?c1)
          (solution nationality spaniard ?n2)
          (solution pet dog ?p1)
          (solution color ivory ?c2)
          (solution color green ?c3)
          (solution drink coffee ?d1)
          (solution drink milk ?d2) 
          (solution smokes old-golds ?s1)
          (solution pet snails ?p2)
          (solution nationality ukrainian ?n3)
          (solution drink tea ?d3)
          (solution nationality norwegian ?n4)
          (solution smokes chesterfields ?s2)
          (solution pet fox ?p3)
          (solution smokes lucky-strikes ?s3)
          (solution drink orange-juice ?d4) 
          (solution nationality japanese ?n5)
          (solution smokes parliaments ?s4)
          (solution pet horse ?p4) 
          (solution smokes kools ?s5)
          (solution color yellow ?c4)
          (solution color blue ?c5)
          (solution drink water ?d5)
          (solution pet zebra ?p5))
  )

(defrule print-solution
  ?f1 <- (solution nationality ?n1 1)
  ?f2 <- (solution color ?c1 1)
  ?f3 <- (solution pet ?p1 1)
  ?f4 <- (solution drink ?d1 1)
  ?f5 <- (solution smokes ?s1 1)
  ?f6 <- (solution nationality ?n2 2)
  ?f7 <- (solution color ?c2 2)
  ?f8 <- (solution pet ?p2 2)
  ?f9 <- (solution drink ?d2 2)
  ?f10 <- (solution smokes ?s2 2)
  ?f11 <- (solution nationality ?n3 3)
  ?f12 <- (solution color ?c3 3)
  ?f13 <- (solution pet ?p3 3)
  ?f14 <- (solution drink ?d3 3)
  ?f15 <- (solution smokes ?s3 3)
  ?f16 <- (solution nationality ?n4 4)
  ?f17 <- (solution color ?c4 4)
  ?f18 <- (solution pet ?p4 4)
  ?f19 <- (solution drink ?d4 4)
  ?f20 <- (solution smokes ?s4 4)
  ?f21 <- (solution nationality ?n5 5)
  ?f22 <- (solution color ?c5 5)
  ?f23 <- (solution pet ?p5 5)
  ?f24 <- (solution drink ?d5 5)
  ?f25 <- (solution smokes ?s5 5)
  =>
  (retract ?f1 ?f2 ?f3 ?f4 ?f5 ?f6 ?f7 ?f8 ?f9 ?f10 ?f11 ?f12 ?f13 ?f14
           ?f15 ?f16 ?f17 ?f18 ?f19 ?f20 ?f21 ?f22 ?f23 ?f24 ?f25)
  (format t "HOUSE | %-11s | %-6s | %-6s | %-12s | %-13s%n" 
            Nationality Color Pet Drink Smokes)
  (format t "--------------------------------------------------------------------%n")
  (format t "  1   | %-11s | %-6s | %-6s | %-12s | %-13s%n" ?n1 ?c1 ?p1 ?d1 ?s1)
  (format t "  2   | %-11s | %-6s | %-6s | %-12s | %-13s%n" ?n2 ?c2 ?p2 ?d2 ?s2)
  (format t "  3   | %-11s | %-6s | %-6s | %-12s | %-13s%n" ?n3 ?c3 ?p3 ?d3 ?s3)
  (format t "  4   | %-11s | %-6s | %-6s | %-12s | %-13s%n" ?n4 ?c4 ?p4 ?d4 ?s4)
  (format t "  5   | %-11s | %-6s | %-6s | %-12s | %-13s%n" ?n5 ?c5 ?p5 ?d5 ?s5)
  (printout t crlf crlf))

(defrule startup
   =>
   (printout t
    "There are five houses, each of a different color, inhabited by men of"  crlf
    "different nationalities, with different pets, drinks, and cigarettes."  crlf
    crlf
    "The Englishman lives in the red house.  The Spaniard owns the dog."     crlf
    "The ivory house is immediately to the left of the green house, where"   crlf
    "the coffee drinker lives.  The milk drinker lives in the middle house." crlf
    "The man who smokes Old Golds also keeps snails.  The Ukrainian drinks"  crlf
    "tea.  The Norwegian resides in the first house on the left.  The"       crlf)
   (printout t
    "Chesterfields smoker lives next door to the fox owner.  The Lucky"      crlf
    "Strike smoker drinks orange juice.  The Japanese smokes Parliaments."   crlf
    "The horse owner lives next to the Kools smoker, whose house is yellow." crlf
    "The Norwegian lives next to the blue house."			                 crlf
    crlf
    "Now, who drinks water?  And who owns the zebra?" crlf crlf)
   (assert (value color red) 
           (value color green) 
           (value color ivory)
           (value color yellow)
           (value color blue)
           (value nationality englishman)
           (value nationality spaniard)
           (value nationality ukrainian) 
           (value nationality norwegian)
           (value nationality japanese)
           (value pet dog)
           (value pet snails)
           (value pet fox)
           (value pet horse)
           (value pet zebra)
           (value drink water)
           (value drink coffee)
           (value drink milk)
           (value drink orange-juice)
           (value drink tea)
           (value smokes old-golds)
           (value smokes kools)
           (value smokes chesterfields)
           (value smokes lucky-strikes)
           (value smokes parliaments)) 
   )

(defrule generate-combinations
   ?f <- (value ?s ?e)
   =>
   (retract ?f)
   (assert (avh (a ?s) (v ?e) (h 1))
           (avh (a ?s) (v ?e) (h 2))
           (avh (a ?s) (v ?e) (h 3))
           (avh (a ?s) (v ?e) (h 4))
           (avh (a ?s) (v ?e) (h 5))))



