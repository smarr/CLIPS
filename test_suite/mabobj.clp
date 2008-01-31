
;;;======================================================
;;;   Monkees and Bananas Sample Problem
;;;
;;;     This is an extended version of a
;;;     rather common AI planning problem.
;;;     The point is for the monkee to find
;;;     and eat some bananas.
;;;
;;;     CLIPS Version 6.0 thing Example
;;;
;;;     To execute, merely load, reset and run.
;;;======================================================
             
;;;*************
;;;* TEMPLATES *
;;;*************

(defclass thing (is-a USER)
   (role concrete)
   (pattern-match reactive)
   (slot id
      (create-accessor write)
      (propagation no-inherit)
      (type SYMBOL)) 
   (slot location 
      (create-accessor write)
      (type SYMBOL)) 
   (slot on-top-of 
      (create-accessor write)
      (type SYMBOL) 
      (default floor))
   (slot weight
      (create-accessor write)
      (propagation no-inherit)
      (type SYMBOL) 
      (allowed-symbols light heavy)
      (default light)))
                    
(defclass monkey (is-a thing)
   (role concrete)
   (pattern-match reactive)
   (slot location 
      (source composite) 
      (default green-couch))
   (slot holding 
      (create-accessor write)
      (type SYMBOL) 
      (default nothing)))

(defclass chest (is-a USER)
   (role concrete)
   (pattern-match reactive)
   (slot id 
      (create-accessor write)
      (type SYMBOL)) 
   (slot contents 
      (create-accessor write)
      (type SYMBOL)) 
   (slot unlocked-by 
      (create-accessor write)
      (type SYMBOL)))
               
(defclass goal-is-to (is-a USER)
   (role concrete)
   (pattern-match reactive)
   (slot action 
      (create-accessor write)
      (type SYMBOL)
      (allowed-symbols hold unlock eat move on walk-to)) 
   (multislot arguments
      (create-accessor write)
      (type SYMBOL)))
             
;;;*************************
;;;* CHEST UNLOCKING RULES *
;;;*************************

(defrule hold-chest-to-put-on-floor "" 
  (object (is-a goal-is-to) (action unlock) (arguments ?chest))
  (object (is-a thing) (id ?chest) (on-top-of ~floor) (weight light))
  (object (is-a monkey) (holding ~?chest))
  (not (object (is-a goal-is-to) (action hold) (arguments ?chest)))
  =>
  (make-instance of goal-is-to 
     (action hold) (arguments ?chest)))

(defrule put-chest-on-floor "" 
  (object (is-a goal-is-to) (action unlock) (arguments ?chest))
  ?monkey <- (object (is-a monkey)
                     (location ?place)
                     (on-top-of ?on)
                     (holding ?chest))
  ?thing <- (object (is-a thing) (id ?chest))
  =>
  (printout t "Monkey throws the " ?chest " off the " 
              ?on " onto the floor." crlf)
  (modify-instance ?monkey (holding blank))
  (modify-instance ?thing (location ?place)
                          (on-top-of floor)))

(defrule get-key-to-unlock "" 
  (object (is-a goal-is-to) (action unlock) (arguments ?obj))
  (object (is-a thing) (id ?obj) (on-top-of floor))
  (object (is-a chest) (id ?obj) (unlocked-by ?key))
  (object (is-a monkey) (holding ~?key))
  (not (object (is-a goal-is-to) (action hold) (arguments ?key)))
  =>
  (make-instance of goal-is-to 
     (action hold) (arguments ?key)))

(defrule move-to-chest-with-key "" 
  (object (is-a goal-is-to) (action unlock) (arguments ?chest))
  (object (is-a monkey) (location ?mplace) (holding ?key))
  (object (is-a thing) (id ?chest) 
                       (location ?cplace&~?mplace)
                       (on-top-of floor))
  (object (is-a chest) (id ?chest) (unlocked-by ?key))
  (not (object (is-a goal-is-to) (action walk-to) (arguments ?cplace)))
  =>
  (make-instance of goal-is-to
     (action walk-to) (arguments ?cplace)))

(defrule unlock-chest-with-key "" 
  ?goal <- (object (is-a goal-is-to) (action unlock) (arguments ?id))
  ?chest <- (object (is-a chest) (id ?id)
                                 (contents ?contents) 
                                 (unlocked-by ?key))
  (object (is-a thing) (id ?id) (location ?place) (on-top-of ?on))
  (object (is-a monkey) (location ?place)
                        (on-top-of ?on) (holding ?key))
  =>
  (printout t "Monkey opens the " ?id " with the " ?key 
              " revealing the  " ?contents "." crlf)
  (modify-instance ?chest (contents nothing))
  (make-instance of thing 
      (id ?contents) (location ?place) (on-top-of ?id))
  (unmake-instance ?goal))

;;;*********************
;;;* HOLD thing RULES * 
;;;*********************

(defrule unlock-chest-to-hold-thing ""
  (object (is-a goal-is-to) (action hold) (arguments ?obj))
  (object (is-a chest) (id ?chest) (contents ?obj))
  (not (object (is-a goal-is-to) (action unlock) (arguments ?chest)))
  =>
  (make-instance of goal-is-to
     (action unlock) (arguments ?chest)))

(defrule use-ladder-to-hold ""
  (object (is-a goal-is-to) (action hold) (arguments ?obj))
  (object (is-a thing) (id ?obj) (location ?place)
                       (on-top-of ceiling) (weight light))
  (not (object (is-a thing) (id ladder) (location ?place)))
  (not (object (is-a goal-is-to) (action move)
                                 (arguments ladder ?place)))
  =>
  (make-instance of goal-is-to
      (action move) (arguments ladder ?place)))

(defrule climb-ladder-to-hold ""
  (object (is-a goal-is-to) (action hold) (arguments ?obj))
  (object (is-a thing) (id ?obj) (location ?place) (on-top-of ceiling) (weight light))
  (object (is-a thing) (id ladder) (location ?place) (on-top-of floor))
  (object (is-a monkey) (on-top-of ~ladder))
  (not (object (is-a goal-is-to) (action on) (arguments ladder)))
  =>
  (make-instance of goal-is-to
     (action on) (arguments ladder)))

(defrule grab-thing-from-ladder "" 
  ?goal <- (object (is-a goal-is-to) (action hold) (arguments ?id))
  ?thing <- (object (is-a thing) (id ?id) (location ?place) 
                     (on-top-of ceiling) (weight light))
  (object (is-a thing) (id ladder) (location ?place))
  ?monkey <- (object (is-a monkey) (location ?place)
                     (on-top-of ladder) (holding blank))
  =>
  (printout t "Monkey grabs the " ?id "." crlf)
  (modify-instance ?thing (location held)
                          (on-top-of held))
  (modify-instance ?monkey (holding ?id))
  (unmake-instance ?goal))

(defrule climb-to-hold "" 
  (object (is-a goal-is-to) (action hold) (arguments ?obj))
  (object (is-a thing) (id ?obj) (location ?place)
                       (on-top-of ?on&~ceiling) (weight light))
  (object (is-a monkey) (location ?place) (on-top-of ~?on))
  (not (object (is-a goal-is-to) (action on) (arguments ?on)))
  =>
  (make-instance of goal-is-to
     (action on) (arguments ?on)))

(defrule walk-to-hold ""
  (object (is-a goal-is-to) (action hold) (arguments ?obj))
  (object (is-a thing) (id ?obj) (location ?place)
                       (on-top-of ~ceiling) (weight light))
  (object (is-a monkey) (location ~?place))
  (not (object (is-a goal-is-to) (action walk-to) (arguments ?place)))
  =>
  (make-instance of goal-is-to
     (action walk-to) (arguments ?place)))

(defrule drop-to-hold ""
  (object (is-a goal-is-to) (action hold) (arguments ?obj))
  (object (is-a thing) (id ?obj) (location ?place)
                       (on-top-of ?on) (weight light))
  (object (is-a monkey) (location ?place)
                        (on-top-of ?on) (holding ~blank))
  (not (object (is-a goal-is-to) (action hold) (arguments blank)))
  =>
  (make-instance of goal-is-to
     (action hold) (arguments blank)))

(defrule grab-thing "" 
  ?goal <- (object (is-a goal-is-to) (action hold) (arguments ?id))
  ?thing <- (object (is-a thing) (id ?id) (location ?place) 
                     (on-top-of ?on) (weight light))
  ?monkey <- (object (is-a monkey) (location ?place)
                     (on-top-of ?on) (holding blank))
  =>
  (printout t "Monkey grabs the " ?id "." crlf)
  (modify-instance ?thing (location held)
                          (on-top-of held))
  (modify-instance ?monkey (holding ?id))
  (unmake-instance ?goal))

(defrule drop-thing ""  
  ?goal <- (object (is-a goal-is-to) (action hold) (arguments blank))
  ?monkey <- (object (is-a monkey) (location ?place) 
                     (on-top-of ?on) 
                     (holding ?id&~blank))
  ?thing <- (object (is-a thing) (id ?id))
  =>
  (printout t "Monkey drops the " ?id "." crlf)
  (modify-instance ?monkey (holding blank))
  (modify-instance ?thing (location ?place)
                          (on-top-of ?on))
  (unmake-instance ?goal))

;;;*********************
;;;* MOVE thing RULES * 
;;;*********************

(defrule unlock-chest-to-move-thing "" 
  (object (is-a goal-is-to) (action move) (arguments ?obj ?))
  (object (is-a chest) (id ?chest) (contents ?obj))
  (not (object (is-a goal-is-to) (action unlock) (arguments ?chest)))
  =>
  (make-instance of goal-is-to
     (action unlock) (arguments ?chest)))

(defrule hold-thing-to-move ""  
  (object (is-a goal-is-to) (action move) (arguments ?obj ?place))
  (object (is-a thing) (id ?obj) (location ~?place) (weight light))
  (object (is-a monkey) (holding ~?obj))
  (not (object (is-a goal-is-to) (action hold) (arguments ?obj)))
  =>
  (make-instance of goal-is-to
     (action hold) (arguments ?obj)))

(defrule move-thing-to-place "" 
  (object (is-a goal-is-to) (action move) (arguments ?obj ?place))
  (object (is-a monkey) (location ~?place) (holding ?obj))
  (not (object (is-a goal-is-to) (action walk-to) (arguments ?place)))
  =>
  (make-instance of goal-is-to
      (action walk-to) (arguments ?place)))

(defrule drop-thing-once-moved "" 
  ?goal <- (object (is-a goal-is-to) 
                   (action move) (arguments ?id ?place))
  ?monkey <- (object (is-a monkey) (location ?place) (holding ?obj))
  ?thing <- (object (is-a thing) (id ?id) (weight light))
  =>
  (printout t "Monkey drops the " ?id "." crlf)
  (modify-instance ?monkey (holding blank))
  (modify-instance ?thing (location ?place)
                          (on-top-of floor))
  (unmake-instance ?goal))

(defrule already-moved-thing ""
  ?goal <- (object (is-a goal-is-to)
                   (action move) (arguments ?obj ?place))
  (object (is-a thing) (id ?obj) (location ?place))
  =>
  (unmake-instance ?goal))

;;;***********************
;;;* WALK TO PLACE RULES *
;;;***********************

(defrule already-at-place "" 
  ?goal <- (object (is-a goal-is-to)
                   (action walk-to) (arguments ?place))
  (object (is-a monkey) (location ?place))
  =>
  (unmake-instance ?goal))

(defrule get-on-floor-to-walk ""
  (object (is-a goal-is-to) (action walk-to) (arguments ?place))
  (object (is-a monkey) (location ~?place) (on-top-of ~floor))
  (not (object (is-a goal-is-to) (action on) (arguments floor)))
  =>
  (make-instance of goal-is-to
     (action on) (arguments floor)))

(defrule walk-holding-nothing ""
  ?goal <- (object (is-a goal-is-to) 
                   (action walk-to) (arguments ?place))
  ?monkey <- (object (is-a monkey)
                     (location ~?place)
                     (on-top-of floor) (holding blank))
  =>
  (printout t "Monkey walks to " ?place "." crlf)
  (modify-instance ?monkey (location ?place))
  (unmake-instance ?goal))

(defrule walk-holding-thing ""
  ?goal <- (object (is-a goal-is-to)
                   (action walk-to) (arguments ?place))
  ?monkey <- (object (is-a monkey)
                     (location ~?place)
                     (on-top-of floor)
                     (holding ?obj&~blank))
  =>
  (printout t "Monkey walks to " ?place " holding the " ?obj "." crlf)
  (modify-instance ?monkey (location ?place))
  (unmake-instance ?goal))

;;;***********************
;;;* GET ON thing RULES * 
;;;***********************

(defrule jump-onto-floor "" 
  ?goal <- (object (is-a goal-is-to) (action on) (arguments floor))
  ?monkey <- (object (is-a monkey) (on-top-of ?on&~floor))
  =>
  (printout t "Monkey jumps off the " ?on " onto the floor." crlf)
  (modify-instance ?monkey (on-top-of floor))
  (unmake-instance ?goal))

(defrule walk-to-place-to-climb "" 
  (object (is-a goal-is-to) (action on) (arguments ?obj))
  (object (is-a thing) (id ?obj) (location ?place))
  (object (is-a monkey) (location ~?place))
  (not (object (is-a goal-is-to) (action walk-to) (arguments ?place)))
  =>
  (make-instance of goal-is-to
     (action walk-to) (arguments ?place)))

(defrule drop-to-climb "" 
  (object (is-a goal-is-to) (action on) (arguments ?obj))
  (object (is-a thing) (id ?obj) (location ?place))
  (object (is-a monkey) (location ?place) (holding ~blank))
  (not (object (is-a goal-is-to) (action hold) (arguments blank)))
  =>
  (make-instance of goal-is-to
     (action hold) (arguments blank)))

(defrule climb-indirectly "" 
  (object (is-a goal-is-to) (action on) (arguments ?obj))
  (object (is-a thing) (id ?obj) (location ?place) (on-top-of ?on))
  (object (is-a monkey) (location ?place) (on-top-of ~?on&~?obj) (holding blank))
  (not (object (is-a goal-is-to) (action on) (arguments ?on)))
  =>
  (make-instance of goal-is-to
     (action on) (arguments ?on)))

(defrule climb-directly ""  
  ?goal <- (object (is-a goal-is-to) (action on) (arguments ?obj))
  (object (is-a thing) (id ?obj) (location ?place) (on-top-of ?on))
  ?monkey <- (object (is-a monkey) (location ?place)
                     (on-top-of ?on) (holding blank))
  =>
  (printout t "Monkey climbs onto the " ?obj "." crlf)
  (modify-instance ?monkey (on-top-of ?obj))
  (unmake-instance ?goal))

(defrule already-on-thing ""
  ?goal <- (object (is-a goal-is-to) (action on) (arguments ?obj))
  (object (is-a monkey) (on-top-of ?obj))
  =>
  (unmake-instance ?goal))

;;;********************
;;;* EAT thing RULES * 
;;;********************

(defrule hold-to-eat ""
  (object (is-a goal-is-to) (action eat) (arguments ?obj))
  (object (is-a monkey) (holding ~?obj))
  (not (object (is-a goal-is-to) (action hold) (arguments ?obj)))
  =>
  (make-instance of goal-is-to
      (action hold) (arguments ?obj)))

(defrule satisfy-hunger ""
  ?goal <- (object (is-a goal-is-to) (action eat) (arguments ?id))
  ?monkey <- (object (is-a monkey) (holding ?id))
  ?thing <- (object (is-a thing) (id ?id))
  =>
  (printout t "Monkey eats the " ?id "." crlf)
  (modify-instance ?monkey (holding blank))
  (unmake-instance ?goal ?thing))
 
;;;**********************
;;;* INITIAL STATE RULE * 
;;;**********************

(definstances initial-objects ""
  (of monkey (location t5-7) (on-top-of green-couch) (holding blank))
  (of thing (id green-couch) (location t5-7) (weight heavy))
  (of thing (id red-couch) (location t2-2) (weight heavy))
  (of thing (id big-pillow) (location t2-2) (on-top-of red-couch))
  (of thing (id red-chest) (location t2-2) (on-top-of big-pillow))
  (of chest (id red-chest) (contents ladder) (unlocked-by red-key))
  (of thing (id blue-chest) (location t7-7) (on-top-of ceiling))
  (of chest (id blue-chest) (contents bananas) (unlocked-by blue-key))
  (of thing (id blue-couch) (location t8-8) (weight heavy))
  (of thing (id green-chest) (location t8-8) (on-top-of ceiling))
  (of chest (id green-chest) (contents blue-key) (unlocked-by red-key))
  (of thing (id red-key) (location t1-3))
  (of goal-is-to (action eat) (arguments bananas)))
