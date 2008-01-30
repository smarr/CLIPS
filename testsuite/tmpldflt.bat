(deftemplate error1 "Wrong Type"
  (field x (type SYMBOL)
           (default 7)))
(deftemplate error2 "Wrong Type"
  (field x (type SYMBOL)
           (default (+ 3 4))))
(deftemplate error3 "Wrong Type"
  (field x (type SYMBOL)
           (default-dynamic 7)))
(deftemplate error4 "Wrong Type"
  (field x (type SYMBOL)
           (default-dynamic (+ 3 4))))
(deftemplate error5 "Wrong Value"
  (field x (allowed-symbols a b c)
           (default f)))
(deftemplate error6 "Wrong Cardinality"
  (field x (default 1 2 3)))
(deftemplate error7 "Wrong Cardinality"
  (field x (default)))
(deftemplate error8 "Wrong Cardinality"
  (field x (default (create$ a b c))))
(deftemplate error9 "Wrong Cardinality"
  (field x (default (create$))))
(deftemplate error11 "Wrong Cardinality"
  (field x (default-dynamic 1 2 3)))
(deftemplate error12 "Wrong Cardinality"
  (field x (default-dynamic)))
(deftemplate error13 "Wrong Cardinality"
  (field x (default-dynamic (create$ a b c))))
(deftemplate error14 "Wrong Cardinality"
  (field x (default-dynamic (create$))))
(deftemplate error16 "Wrong Cardinality"
  (multifield x (cardinality 3 5)
                (default a b c d e f)))
(deftemplate error17 "Wrong Cardinality"
  (multifield x (cardinality 3 5)
                (default a)))
(deftemplate error18 "Wrong Cardinality"
  (multifield x (cardinality 3 5)
                (default (create$ a b c d e f))))
(deftemplate error19 "Wrong Cardinality"
  (multifield x (cardinality 3 5)
                (default (create$ a))))
(deftemplate error20 "Wrong Cardinality"
  (multifield x (cardinality 3 5)
                (default (create$ a) (create$ a))))
(deftemplate error21 "Wrong Cardinality"
  (multifield x (cardinality 3 5)
                (default (create$ a b c) (create$ a b c))))
(deftemplate error22 "Wrong Cardinality"
  (multifield x (cardinality 3 5)
                (default a b c d e f)))
(deftemplate error23 "Wrong Cardinality"
  (multifield x (cardinality 3 5)
                (default a)))
(deftemplate error24 "Wrong Cardinality"
  (multifield x (cardinality 3 5)
                (default (create$ a b c d e f))))
(deftemplate error25 "Wrong Cardinality"
  (multifield x (cardinality 3 5)
                (default (create$ a))))
(deftemplate error26 "Wrong Cardinality"
  (multifield x (cardinality 3 5)
                (default (create$ a) (create$ a))))
(deftemplate error27 "Wrong Cardinality"
  (multifield x (cardinality 3 5)
                (default (create$ a b c) (create$ a b c))))
(deftemplate error28 "Wrong Cardinality"
  (multifield x (cardinality 3 5)
                (default)))
(deftemplate error29 "Wrong Cardinality"
  (multifield x (cardinality 3 5)
                (default-dynamic)))
(deftemplate error30 "Syntax Error"
  (field x (default ?NONE 10)))
(deftemplate error31 "Syntax Error"
  (field x (default ?DERIVE 10)))
(deftemplate error32 "Syntax Error"
  (field x (default 10 ?DERIVE)))
(deftemplate error33 "Wrong Type"
  (multifield x (allowed-symbols a b c)
                (default 8 7 a 9 d)))
(list-deftemplates)
(watch facts)                
(deftemplate foo1 (field x))
(assert (foo1))
(deftemplate foo2 (field x (type LEXEME NUMBER INSTANCE FACT-ADDRESS EXTERNAL-ADDRESS)))
(assert (foo2))
(deftemplate foo3 (field x (type STRING NUMBER INSTANCE FACT-ADDRESS EXTERNAL-ADDRESS)))
(assert (foo3))
(deftemplate foo4 (field x (type NUMBER INSTANCE FACT-ADDRESS EXTERNAL-ADDRESS)))
(assert (foo4))
(deftemplate foo5 (field x (type INTEGER INSTANCE FACT-ADDRESS EXTERNAL-ADDRESS)))
(assert (foo5))
(deftemplate foo6 (field x (type FLOAT INSTANCE FACT-ADDRESS EXTERNAL-ADDRESS)))
(assert (foo6))
(deftemplate foo7 (field x (type INSTANCE FACT-ADDRESS EXTERNAL-ADDRESS)))
(assert (foo7))
(deftemplate foo8 (field x (type INSTANCE-ADDRESS FACT-ADDRESS EXTERNAL-ADDRESS)))
(assert (foo8))
(deftemplate foo9 (field x (type FACT-ADDRESS EXTERNAL-ADDRESS)))
(assert (foo9))
(deftemplate foo10 (field x (type EXTERNAL-ADDRESS)))
(assert (foo10))
(deftemplate foo11 (field x (type INTEGER) (allowed-integers 3 4 5)))
(assert (foo11))
(deftemplate foo12 (field x (type FLOAT) (range 9.8 20.1)))
(assert (foo12))
(deftemplate foo13 (field x (type INTEGER) (range ?VARIABLE 43)))
(assert (foo13))
(deftemplate foo14 (field x (type INTEGER) (range 28 ?VARIABLE)))
(assert (foo14))
(deftemplate foo15 (multifield x))
(assert (foo15))
(deftemplate foo16 (multifield x (cardinality ?VARIABLE 5)))
(assert (foo16))
(deftemplate foo17 (multifield x (cardinality 3 5)))
(assert (foo17))
(deftemplate foo18 (multifield x (cardinality 3 5) (type INTEGER)))
(assert (foo18))
(deftemplate foo19 (multifield x (cardinality 3 5) (allowed-symbols a b c)))
(assert (foo19))
(deftemplate foo20 (multifield x (cardinality 3 5) (type INTEGER) (range 8 9)))
(assert (foo20))
(deftemplate foo21 (field x (default ?NONE)))
(assert (foo21))
(setgen 1)
(deftemplate foo22 (field x (default b)) (field y (default (gensym))))
(assert (foo22))
(assert (foo22))
(deftemplate foo23 (field x (default-dynamic b)) (field y (default-dynamic (gensym))))
(assert (foo23))
(assert (foo23))
(unwatch facts)
