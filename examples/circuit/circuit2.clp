;;;======================================================
;;;   Example Circuit #2
;;;
;;;     An example circuit to be loaded for use with
;;;     the "electronic.clp" example program. This 
;;;     circuit implements the logical functions
;;;     (not (and S1 S2)) and (or (not S1) (not S2))
;;;     which are equivalent and uses them as input
;;;     to an XOR gate. Thus the output of the
;;;     XOR gate X1 should always be zero and the
;;;     LED L1 should also always be zero.
;;;
;;; LEGEND
;;; ------------
;;; S = Source
;;; P = Splitter
;;; N = NOT Gate
;;; A = AND Gate
;;; O = OR Gate
;;; X = XOR Gate
;;; L = LED
;;; 
;;;
;;;          /--------------\           
;;; S1>--P1>-|               A1>------N1>----------\
;;;          |        /-----/                       X1>---L1
;;;          |        |                    /-------/  
;;;          \--------)------N2>-----\     |      
;;;                   |               O1>--/
;;;          /--------/          /---/
;;; S2>--P2>-|                   |
;;;          \---------------N3>-/           
;;;
;;;======================================================

(definstances circuit
  (S-1 of SOURCE)
  (S-2 of SOURCE)
  (P-1 of SPLITTER)
  (P-2 of SPLITTER)
  (A-1 of AND-GATE)
  (N-1 of NOT-GATE)
  (N-2 of NOT-GATE)
  (N-3 of NOT-GATE)
  (O-1 of OR-GATE)
  (X-1 of XOR-GATE)
  (L-1 of LED))       

(deffunction connect-circuit ()
  (connect [S-1] [P-1])
  (connect [S-2] [P-2])
  (connect [P-1] 1 [A-1] 1)
  (connect [P-1] 2 [N-2])
  (connect [P-2] 1 [A-1] 2)
  (connect [P-2] 2 [N-3])
  (connect [A-1] [N-1])
  (connect [N-2] [O-1] 1)
  (connect [N-3] [O-1] 2)
  (connect [N-1] [X-1] 1)
  (connect [O-1] [X-1] 2)
  (connect [X-1] [L-1]))



