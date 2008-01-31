;;;************************************************************
;;; DEFTEMPLATE ERROR CHECKING
;;;
;;; This file tests a number of common errors which can be
;;; made with deftemplates. Among the errors tested are
;;;   (1) type/range attribute conflicts
;;;   (2) range/allowed-... attribute conflicts
;;;   (3) type/allowed-... attribute conflicts
;;;   (4) type/default attribute conflicts
;;;   (5) range/default attribute conflicts
;;;   (6) allowed-.../default attribute conflicts
;;;   (7) redefinition of a field
;;;   (8) more than one multifield slot
;;;   (9) redefinition of an attribute
;;;  (10) illegal attribute values
;;;
;;; When the file is loaded, only those deftemplates with
;;; the aok prefix should be displayed with a list-deftemplates
;;; command is executed. Any deftemplates shown with the prefix
;;; bad indicate that an error was not detected. The number of
;;; deftemplates listed should also be checked to make sure all
;;; of the aok deftemplates loaded properly.
;;;************************************************************

;;;********************************************
;;; These deftemplate check type/range conflict
;;;********************************************

(deftemplate bad-foo01 (field x (type SYMBOL) (range 1 3)))
(deftemplate bad-foo02 (field x (range 1 3) (type SYMBOL)))
(deftemplate bad-foo03 (field x (type INTEGER) (range 1.0 3)))
(deftemplate bad-foo04 (field x (range 1 3.0) (type FLOAT)))

(deftemplate aok-foo01 (field x (range 1.0 3.0) (type FLOAT)))
(deftemplate aok-foo02 (field x (range 1  3.0) (type NUMBER)))
(deftemplate aok-foo03 (field x (range 1 3)))
(deftemplate aok-foo04 (field x (range 1 3) (type INTEGER)))

;;;*****************************************************
;;; These deftemplates check range/allowed-... conflicts
;;;*****************************************************

(deftemplate bad-foo05 (field x (range 1 3.0) (allowed-integers 4 5 6)))
(deftemplate bad-foo06 (field x (range 1 3) (allowed-floats 4.1 5.2)))
(deftemplate bad-foo07 (field x (allowed-integers 4 5 6) (range 1 3.0)))
(deftemplate bad-foo08 (field x (allowed-numbers 4.1 5) (range 1 3)))

(deftemplate aok-foo05 (field x (range 1 3.0) (allowed-symbols a b)))

;;;****************************************************
;;; These deftemplates check type/allowed-... conflicts
;;;****************************************************

(deftemplate bad-foo09 (field x (type INTEGER) (allowed-floats 1.0 2.0)))
(deftemplate bad-foo10 (field x (type FLOAT) (allowed-integers 1 2)))
(deftemplate bad-foo11 (field x (allowed-floats 1.0 2.0) (type INTEGER)))
(deftemplate bad-foo12 (field x (allowed-integers 1 2) (type FLOAT)))
(deftemplate bad-foo13 (field x (allowed-strings "a" "b") (type SYMBOL)))
(deftemplate bad-foo13 (field x (allowed-symbols a b) (type STRING)))
(deftemplate bad-foo12 (field x (allowed-integers 1 2) (type SYMBOL)))

(deftemplate aok-foo06 (field x (type INTEGER FLOAT) 
                                (allowed-integers 1 2) 
                                (allowed-floats 4.0 5.0)))
(deftemplate aok-foo07 (field x (type NUMBER) 
                                (allowed-integers 1 2) 
                                (allowed-floats 4.0 5.0)))
(deftemplate aok-foo08 (field x (allowed-integers 1 2) 
                                (allowed-floats 4.0 5.0)
                                (type INTEGER FLOAT)))
(deftemplate aok-foo09 (field x (allowed-integers 1 2) 
                                (allowed-floats 4.0 5.0)
                                (type NUMBER)))

;;;*****************************************************************
;;; These deftemplates check default conflicts with other attributes
;;;*****************************************************************

(deftemplate bad-foo13 (field x (type INTEGER) (default 1.0)))
(deftemplate bad-foo14 (field x (type FLOAT) (default 1)))
(deftemplate bad-foo15 (field x (type NUMBER) (default a)))
(deftemplate bad-foo16 (field x (range 1 3) (default 5)))
(deftemplate bad-foo17 (field x (allowed-integers 2 3 4) (default 1)))
(deftemplate bad-foo18 (field x (default 1.0)(type INTEGER)))
(deftemplate bad-foo19 (field x (default 5) (type SYMBOL)))
(deftemplate bad-foo20 (field x (default 5.0) (range 10.0 30)))
(deftemplate bad-foo21 (field x (allowed-strings "a" "b") (default "c")))
(deftemplate bad-foo22 (multifield x (default 3 4 a) (type INTEGER)))
(deftemplate bad-foo23 (multifield x (default 10.0 5.0) (range 10.0 30.0)))
(deftemplate bad-foo24 (multifield x (allowed-strings "a" "b") (default "a" "b" "c")))

(deftemplate aok-foo10 (field x (type INTEGER) (default 1)))
(deftemplate aok-foo11 (field x (type FLOAT) (default 1.6)))
(deftemplate aok-foo12 (field x (type SYMBOL) (default a)))
(deftemplate aok-foo13 (field x (range 1 3) (default 1)))
(deftemplate aok-foo14 (field x (allowed-integers 2 3 4) (default 4)))
(deftemplate aok-foo15 (field x (default 1.0) (type FLOAT)))
(deftemplate aok-foo16 (field x (default 5) (type INTEGER)))
(deftemplate aok-foo17 (field x (default 29) (range 10.0 30)))
(deftemplate aok-foo18 (field x (allowed-strings "a" "b") (default "b")))
(deftemplate aok-foo19 (multifield x (default 3 4 5) (type INTEGER)))
(deftemplate aok-foo20 (multifield x (default 10.0 30.0) (range 10.0 30.0)))
(deftemplate aok-foo21 (multifield x (allowed-strings "a" "b") (default "a" "b")))

;;;************************************************
;;; These deftemplates check redefinition of fields
;;; (some of these are more than one multifield)
;;;************************************************

(deftemplate bad-foo25 (field x (type INTEGER)) (field x (type SYMBOL)))
(deftemplate bad-foo26 (multifield x (type INTEGER)) (field x (type SYMBOL)))
(deftemplate bad-foo27 (field x (type INTEGER)) (multifield x (type SYMBOL)))
(deftemplate bad-foo28 (multifield x (type INTEGER)) (multifield x (type SYMBOL)))
(deftemplate bad-foo29 (field x) (field y) (field x))
(deftemplate bad-foo30 (field x) (field y) (field x) (field y))
(deftemplate bad-foo31 (multifield x) (field y) (field x))
(deftemplate bad-foo32 (field x) (multifield y) (field x) (multifield y))

;;;********************************************************
;;; These deftemplates check more than one multifield slot
;;;********************************************************

(deftemplate aok-foo21a (field x) (field y) (multifield z) (multifield q))
(deftemplate aok-foo21b (multifield x) (field y) (field z) (multifield q))
(deftemplate aok-foo21c (field x) (multifield y) (multifield z) (field q))
(deftemplate aok-foo21d (multifield x) (multifield y) (field z) (field q))

;;;******************************************************
;;; These deftemplates check redefinition of an attribute
;;;******************************************************

(deftemplate bad-foo37 (field x (type INTEGER) (range 1 2) (type SYMBOL)))
(deftemplate bad-foo38 (multifield x (range 0 3) (range 2 4) (type INTEGER)))
(deftemplate bad-foo39 (field x (range 1 10) 
                                (default 3) 
                                (type INTEGER) 
                                (default 5)))
(deftemplate bad-foo40 (multifield x (type SYMBOL) 
                                      (default a b) 
                                      (allowed-values a b c x y z)
                                      (default x y z)))
(deftemplate bad-foo41 (field x (allowed-numbers 1 3.5 10 20.7) 
                                (type NUMBER)
                                (allowed-floats 3.5 20.7)
                                (default ?NONE)
                                (allowed-integers 1 10)))
(deftemplate bad-foo42 (multifield x (allowed-integers 1 10)
                                      (allowed-numbers 1 3.5 10 20.7) 
                                      (type NUMBER)
                                      (allowed-floats 3.5 20.7)
                                      (default ?NONE)))
(deftemplate bad-foo43 (multifield x (allowed-floats 3.5 20.7)
                                      (default ?NONE)
                                      (allowed-integers 1 10)
                                      (allowed-numbers 1 3.5 10 20.7) 
                                      (type NUMBER)))
(deftemplate bad-foo44 (field x (allowed-symbols a b c) 
                                (type SYMBOL)
                                (allowed-symbols d e f)))
(deftemplate bad-foo45 (field x (type INTEGER)
                                (allowed-integers 1 2 3)
                                (allowed-integers 4 5 6)))
(deftemplate bad-foo46 (field x (allowed-floats 1.0 2.0 3.0)
                                (allowed-floats 4.0 5.0 6.0)
                                (type FLOAT)))
(deftemplate bad-foo47 (field x (allowed-integers 4 5 6)
                                (allowed-values 1 2 3)))
(deftemplate bad-foo48 (multifield x (allowed-values 1.0 2.0 3.0)
                                      (allowed-floats 4.0 5.0 6.0)))
(deftemplate bad-foo49 (field x (allowed-symbols d e f)
                                (allowed-values a b c)))
(deftemplate bad-foo50 (multifield x (allowed-values "a" "b")
                                      (allowed-strings "d" "e")))

;;;**************************************************
;;; These deftemplates check illegal attribute values
;;;**************************************************

(deftemplate bad-foo51 (field x  (allowed-symbols d 1.0 f)))
(deftemplate bad-foo52 (multifield x  (allowed-integers 1 2.0 3)))
(deftemplate bad-foo53 (field x  (allowed-floats 1.0 2.0 3)))
(deftemplate bad-foo54 (multifield x  (allowed-strings "a" "b" c)))
(deftemplate bad-foo55 (multifield x  (allowed-numbers 1 2.0 3 x)))
(deftemplate bad-foo56 (field x  (allowed-symbols d e f ?VARIABLE)))
(deftemplate bad-foo57 (multifield x  (allowed-integers ?VARIABLE 1 2 3)))
(deftemplate bad-foo58 (field x  (allowed-floats 1.0 ?VARIABLE 2.0 3.0)))
(deftemplate bad-foo59 (multifield x  (allowed-strings ?VARIABLE "a" "b" "c")))
(deftemplate bad-foo60 (multifield x  (allowed-numbers 1 2.0 3 ?VARIABLE)))
(deftemplate bad-foo61 (multifield x  (allowed-integers ?NONE 1 2 3)))
(deftemplate bad-foo62 (field x  (allowed-symbols d e f ?NONE)))
(deftemplate bad-foo63 (field x  (type ?VARIABLE FLOAT)))
(deftemplate bad-foo64 (multifield x  (type FLOAT ?VARIABLE)))
(deftemplate bad-foo65 (field x  (type FLOAT NUMBER)))
(deftemplate bad-foo66 (multifield x  (type NUMBER INTEGER)))
(deftemplate bad-foo67 (field x  (type ?NONE FLOAT)))
(deftemplate bad-foo68 (multifield x  (type FLOAT ?NONE)))
(deftemplate bad-foo69 (field x  (default ?NONE x)))
(deftemplate bad-foo70 (multifield x  (default x ?NONE)))
(deftemplate bad-foo71 (field x  (default ?VARIABLE x)))
(deftemplate bad-foo72 (multifield x  (default x ?VARIABLE)))
(deftemplate bad-foo73 (field x (range 1 a)))
(deftemplate bad-foo74 (multifield x (range a 1)))
(deftemplate bad-foo75 (field x (range 1)))
(deftemplate bad-foo76 (field x (range a b)))
(deftemplate bad-foo77 (multifield x (range 1 ?NONE)))
(deftemplate bad-foo78 (field x (range ?NONE 1)))
(deftemplate bad-foo79 (field x (range ?NONE ?NONE)))
(deftemplate bad-foo80 (multifield x (range 8 1)))
(deftemplate bad-foo81 (field x (range 8.0 1)))
(deftemplate bad-foo82 (multifield x (range 8.0 1.0)))
(deftemplate bad-foo83 (field x (range 8 1.0)))
(deftemplate bad-foo84 (multifield x (range ?VARIABLE)))
(deftemplate bad-foo85 (field x (type STRING SYMBOL STRING)))
(deftemplate bad-foo86 (multifield x (type SYMBOL SYMBOL STRING STRING)))
(deftemplate bad-foo87 (field x (default)))
(deftemplate bad-foo88 (field x (range)))
(deftemplate bad-foo89 (field x (type)))
(deftemplate bad-foo90 (field x (allowed-symbols)))
(deftemplate bad-foo91 (field x (allowed-integers)))
(deftemplate bad-foo92 (field x (allowed-numbers)))

(deftemplate aok-foo22 (field x  (type INTEGER FLOAT SYMBOL STRING INSTANCE)))
(deftemplate aok-foo23 (field x  (type NUMBER SYMBOL STRING INSTANCE)))
(deftemplate aok-foo24 (field x  (type ?VARIABLE)))
(deftemplate aok-foo25 (field x  (allowed-symbols d f)))
(deftemplate aok-foo26 (multifield x  (allowed-integers 1 3)))
(deftemplate aok-foo27 (field x  (allowed-floats 1.0 2.0)))
(deftemplate aok-foo28 (multifield x  (allowed-strings "a" "b" "c")))
(deftemplate aok-foo29 (multifield x  (allowed-numbers 1 2.0 3)))
(deftemplate aok-foo30 (field x  (allowed-symbols ?VARIABLE)))
(deftemplate aok-foo31 (multifield x  (allowed-integers ?VARIABLE)))
(deftemplate aok-foo32 (field x  (allowed-floats ?VARIABLE)))
(deftemplate aok-foo33 (multifield x  (allowed-strings ?VARIABLE)))
(deftemplate aok-foo34 (multifield x  (allowed-numbers ?VARIABLE)))
(deftemplate aok-foo35 (multifield x  (default ?NONE)))
(deftemplate aok-foo36 (multifield x  (default)))
(deftemplate aok-foo37 (field x  (default ?NONE)))
(deftemplate aok-foo38 (field x (range ?VARIABLE ?VARIABLE)))
(deftemplate aok-foo39 (multifield x (range 1 ?VARIABLE)))
(deftemplate aok-foo40 (field x (range ?VARIABLE 8)))
(deftemplate aok-foo41 (multifield x (range 1.0 ?VARIABLE)))
(deftemplate aok-foo42 (field x (range ?VARIABLE 8.0)))
(deftemplate aok-foo43 (field x (range 1 8)))
(deftemplate aok-foo44 (multifield x (range 1.0 8)))
(deftemplate aok-foo45 (multifield x (range 1.0 8.0)))
(deftemplate aok-foo46 (multifield x (range 1 8.0)))

;;;**************************************************
;;; Additional Tests
;;;**************************************************

(deftemplate bad-foo93 (field x (type LEXEME SYMBOL)))
(deftemplate bad-foo94 (field x (type STRING LEXEME)))

(deftemplate aok-foo47 (field x  (type LEXEME EXTERNAL-ADDRESS)))

