;;;======================================================
;;;   Example Circuit #3
;;;
;;;     An example circuit to be loaded for use with
;;;     the "electronic.clp" example program.
;;;
;;; LEGEND
;;; ------------
;;; S = Source
;;; P = Splitter
;;; N = NOT Gate
;;; A = AND Gate
;;; D = NAND Gate
;;; O = OR Gate
;;; X = XOR Gate
;;; L = LED
;;; 
;;;
;;;          /----------\           
;;; S1>--P1>-|           O1>-------------\           /--N3>--L1
;;;          |      /---/                 X2>----P4>-|
;;;          |      |                /---/           \-------L2
;;;          \------)----N1>---\     |      
;;;                 |           X1>--/
;;;          /--N2>-/  /-------/
;;; S2>--P2>-|         |    
;;;          \---------)-------\
;;;                    |        A1>---------\
;;;          /---------/  /----/            |
;;; S3>--P3>-|            |                 |        
;;;          \------------)----\            \---\
;;;                       |     D1>------\       D2>---------L3
;;; S4>-------------------/    /          O2>---/
;;;                            |   /-----/
;;; S5>------------------------/   |
;;;                                |
;;; S6>----------------------------/    
;;;
;;;======================================================

(definstances circuit
  (S-1 of SOURCE)
  (S-2 of SOURCE)
  (S-3 of SOURCE)
  (S-4 of SOURCE)
  (S-5 of SOURCE)
  (S-6 of SOURCE)
  (P-1 of SPLITTER)
  (P-2 of SPLITTER)
  (P-3 of SPLITTER)
  (P-4 of SPLITTER)
  (N-1 of NOT-GATE)
  (N-2 of NOT-GATE)
  (N-3 of NOT-GATE)
  (O-1 of OR-GATE)
  (O-2 of OR-GATE)
  (X-1 of XOR-GATE)
  (X-2 of XOR-GATE)
  (A-1 of AND-GATE)
  (D-1 of NAND-GATE)
  (D-2 of NAND-GATE)
  (L-1 of LED)
  (L-2 of LED)
  (L-3 of LED))

(deffunction connect-circuit ()
  (connect [S-1] [P-1])
  (connect [S-2] [P-2])
  (connect [S-3] [P-3])
  (connect [S-4] [A-1] 2)
  (connect [S-5] [D-1] 2)
  (connect [S-6] [O-2] 2)
  (connect [P-1] 1 [O-1] 1)
  (connect [P-1] 2 [N-1])
  (connect [P-2] 1 [N-2])
  (connect [P-2] 2 [A-1] 1)
  (connect [P-3] 1 [X-1] 2)
  (connect [P-3] 2 [D-1] 1)
  (connect [N-1] [X-1] 1)
  (connect [N-2] [O-1] 2)
  (connect [O-1] [X-2] 1)
  (connect [X-1] [X-2] 2)
  (connect [A-1] [D-2] 1)
  (connect [D-1] [O-2] 1)
  (connect [X-2] [P-4])
  (connect [O-2] [D-2] 2)
  (connect [P-4] 1 [N-3])
  (connect [P-4] 2 [L-2])
  (connect [D-2] [L-3])
  (connect [N-3] [L-1]))
