;;;************************************************************
;;; PATTERN ADDITION CHECKING
;;;
;;; This file tests to see if the addition of patterns to
;;; rules works correctly. The pattern (initial-fact) is
;;; added to and/not CEs thats first CE is a test CE or
;;; to and CEs thats first CE is a not CE.
;;;
;;; To test perform a (clear), (load ...), and (agenda)
;;; The agenda command should show of list of 24 activations,
;;; all beginning with the prefix should-fire-...
;;;************************************************************

(defrule should-fire-1
  (declare (salience 1))
  =>)

(defrule should-not-fire-1a
  (not (and (initial-fact)))
  =>)

(defrule should-not-fire-1b
  (not (initial-fact))
  =>)

; Single test CE

(defrule should-fire-2a
  (declare (salience 2))
  (test (> 5 3))
  =>)

(defrule should-fire-2b
  (declare (salience 3))
  (initial-fact)
  (test (> 5 3))
  =>)

(defrule should-not-fire-2a
  (test (< 5 3))
  =>)

(defrule should-not-fire-2b
  (initial-fact)
  (test (< 5 3))
  =>)

; Single test CE within and CE

(defrule should-fire-3a
  (declare (salience 4))
  (and (test (> 5 3)))
  =>)

(defrule should-fire-3b
  (declare (salience 5))
  (and (initial-fact)
       (test (> 5 3)))
  =>)

(defrule should-not-fire-3a
  (and (test (< 5 3)))
  =>)

(defrule should-not-fire-3b
  (and (initial-fact)
       (test (< 5 3)))
  =>)

; Single test CE within and CE within not CE

(defrule should-fire-4a
  (declare (salience 6))
  (not (and (test (< 5 3))))
  =>)
  
(defrule should-fire-4b
  (declare (salience 7))
  (not (test (< 5 3)))
  =>)
  
(defrule should-fire-4c
  (declare (salience 8))
  (not (and (initial-fact) 
            (test (< 5 3))))
  =>)

(defrule should-fire-4d
  (declare (salience 9))
  (initial-fact)
  (not (and (initial-fact) 
            (test (< 5 3))))
  =>)
  
(defrule should-fire-4e
  (declare (salience 10))
  (not (and (test (< 5 3)) 
            (initial-fact)))
  =>)
  
(defrule should-not-fire-4a
  (not (and (test (> 5 3))))
  =>)

(defrule should-not-fire-4b
  (not (and (initial-fact) 
            (test (> 5 3))))
  =>)
  
(defrule should-not-fire-4c
  (initial-fact)
  (not (and (initial-fact) 
            (test (> 5 3))))
  =>)

(defrule should-not-fire-4d
  (not (test (> 5 3)))
  =>)
  
(defrule should-not-fire-4e
  (not (and (test (> 5 3)) 
            (initial-fact)))
  =>)
  
; Single test CE within two not CEs
  
(defrule should-fire-5a
  (declare (salience 11))
  (exists (test (< 3 5)))
  =>)

(defrule should-fire-5b
  (declare (salience 12))
  (not (not (test (< 3 5))))
  =>)

(defrule should-fire-5c
  (declare (salience 13))
  (initial-fact)
  (exists (test (< 3 5)))
  =>)

(defrule should-fire-5d
  (declare (salience 14))
  (initial-fact)
  (not (not (test (< 3 5))))
  =>)
  
(defrule should-fire-5e
  (declare (salience 15))
  (initial-fact)
  (exists (and (initial-fact) (test (< 3 5))))
  =>)

(defrule should-fire-5f
  (declare (salience 16))
  (initial-fact)
  (not (not (and (initial-fact) (test (< 3 5)))))
  =>)

(defrule should-not-fire-5a
  (exists (test (> 3 5)))
  =>)

(defrule should-not-fire-5b
  (not (not (test (> 3 5))))
  =>)

(defrule should-not-fire-5c
  (initial-fact)
  (exists (test (> 3 5)))
  =>)

(defrule should-not-fire-5d
  (initial-fact)
  (not (not (test (> 3 5))))
  =>)
  
(defrule should-not-fire-5e
  (initial-fact)
  (exists (and (initial-fact) (test (> 3 5))))
  =>)

(defrule should-not-fire-5f
  (initial-fact)
  (not (not (and (initial-fact) (test (> 3 5)))))
  =>)

; Forall CE

(defrule should-fire-6a
  (declare (salience 17))
  (forall (initial-fact) 
          (initial-fact))
  =>)

(defrule should-fire-6b
  (declare (salience 18))
  (forall (initial-fact) 
          (test (> 5 3)))
  =>)

(defrule should-fire-6c
  (declare (salience 19))
  (forall (test (> 5 3)) 
          (initial-fact))
  =>)

(defrule should-fire-6d
  (declare (salience 20))
  (forall (test (> 5 3)) 
          (test (> 5 3)))
  =>)

(defrule should-fire-6e
  (declare (salience 21))
  (forall (test (< 5 3)) 
          (initial-fact))
  =>)

(defrule should-fire-6f
  (declare (salience 22))
  (forall (test (< 5 3)) 
          (test (> 5 3)))
  =>)

(defrule should-fire-6g
  (declare (salience 23))
  (forall (test (< 5 3)) 
          (test (< 5 3)))
  =>)

(defrule should-not-fire-6a
  (forall (initial-fact) 
          (test (< 5 3)))
  =>)

(defrule should-not-fire-6b
  (forall (test (> 5 3)) 
          (test (< 5 3)))
  =>)

