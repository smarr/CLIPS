;;; This file contains a series of rules to test the basic CLIPS capability.
;;; The testing conforms to the content and capabilities as outlined in the
;;; CLIPS Basic Programming Guide.

;;; Section 2.2 Facts

;;; Field values may be numbers, words or strings.  Assumption:
;;; If a field value in a field tests correctly, then it is valid for all
;;; fields in a fact.

;;; Section 2.5.1 Defining Rules

;;; The defrule construct is employed checking pattern matching on the LHS
;;; of the rule.  Pattern matching involves only literal fields.  Additionally,
;;; assert and retract are employed on the RHS.

;;; Section 2.5.2 Defining Initial Facts

;;; The deffacts construct is employed to assert initial facts into the
;;; knowledge base.  Facts contain numbers, words and strings as field values.
;;; These initial facts are subsequently retracted.

;;; 3.0 Conditions

;;; 3.1 Literal Patterns

;;; Literal pattern matching is employed in verifing the functionality of the
;;; deffacts construct and in the manipulation of control facts.

;;; CR0077 implemented 07/09/90


;;;=============== INITIAL FACTS FOR LITERAL PATTERN MATCHING =================


(deffacts field-values-1 "loading of numbers"
 (number 237)
 (number 15.09)
 (number +12.9)
 (number -32.3e-7))

(deffacts field-values-2 "loading of words"
 (word foo)
 (word Hello)
 (word B76-HI)
 (word bad-value)
 (word specialchar!@#$%^*_+{}:>`-=\[]',./))

(deffacts field-values-3 "loading of strings"
 (string "foo")
 (string "a and b")
 (string "1 number")
 (string "a\"quote")
;  should be "two crlf lines"   ; DR0152
 (string "two lines")
 (string "specialchar~!@#$%^&*()_+|{}:\"<>? `-=\\[];',./"))

(deffacts field-values-4 "loading of delimeter test subset"
 (delimeter ab<cd)   
 (delimeter ab   ;xy       
               cw)        
 (delimeter a"a")  
 (delimeter ab
cd))


;(deffacts no-values)     ; DR0151

;;;============= RULES FOR STARTING AND STOPPING TEST =============


(defrule start-test                                                     ;CR0020
=>                                                                      ;CR0020
 (assert (begin field-values)))                                         ;CR0020

(defrule end-test
 (declare (salience -100))
 ?f1 <- (end additional-action-functions)
=>
 (retract ?f1))


;;;================= LITERAL PATTERN MATCHING ==========================


(defrule field-values-matching-1 "testing of numbers"
 (begin field-values)
 (number 237)
 (number 15.09)
 (number +12.9)
 (number -32.3e-7)
=>
 (assert (successful "number field values")))

(defrule field-values-matching-2 "testing of words"              ;CR0001
 (begin field-values)                                            ;CR0001
 (word foo)                                                      ;CR0001
 (word Hello)                                                    ;CR0001
 (word B76-HI)                                                   ;CR0001
 (word bad-value)                                                ;CR0001
 (word specialchar!@#$%^*_+{}:>`-=\[]',./)                       ;CR0001
=>                                                               ;CR0001
 (assert (successful "word field values")))                      ;CR0001

(defrule field-values-matching-3 "testing of strings"            ;CR0001
 (begin field-values)                                            ;CR0001
 (string "foo")                                                  ;CR0001
 (string "a and b")                                              ;CR0001
 (string "1 number")                                             ;CR0001
 (string "a\"quote")                                             ;CR0001
;             should be "two crlf lines"                       ; DR0152 
 (string "two lines")                                                          ;CR0001 CR0004
 (string "specialchar~!@#$%^&*()_+|{}:\"<>? `-=\\[];',./")       ;CR0001 CR0004
=>                                                               ;CR0001
 (assert (successful "string field values")))                    ;CR0001

(defrule field-values-matching-4 "testing of delimeters"         ;CR0001
 (begin field-values)                                            ;CR0001
 (delimeter ab cw)                                               ;CR0001
 (delimeter ab <cd)                                              ;CR0001
 (delimeter a  "a")                                         ;CR0001
 (delimeter ab cd)                                               ;CR0001
=>                                                               ;CR0001
 (assert (successful "delimeter test")))                         ;CR0001

(defrule field-values-clean-up "wildcard testing coming next"
 (declare (salience -10)) ; Added by Gary Riley
 ?f1 <- (successful "number field values")
 ?f2 <- (successful "word field values")
 ?f3 <- (successful "string field values")
 ?f4 <- (successful "delimeter test")
 ?f5 <- (begin field-values)
=>
 (retract ?f1 ?f2 ?f3 ?f4 ?f5)
 (printout t "literal pattern matching successful" crlf)       ;CR0020
 (assert (end field-values)))

(defrule error-matching-literal-patterns
 (declare (salience -30))
 (begin field-values)
=>
 (printout t "error in literal pattern matching" crlf))

(defrule clean-up-number-facts
 (declare (salience -20)) ; Added by Gary Riley
 ?f1 <- (number ?)
 (end field-values)
=>
 (retract ?f1))

(defrule error-clean-up-number-facts
 (declare (salience -30)) ; Added by Gary Riley
 (number ?)
 (end field-values)
=>
 (printout t "error clean-up-number-facts" crlf))

(defrule clean-up-word-facts
 (declare (salience -20)) ; Added by Gary Riley
 ?f1 <- (word $?)
 (end field-values)
=>
 (retract ?f1))

(defrule error-clean-up-word-facts
 (declare (salience -30)) ; Added by Gary Riley
 (word $?)
 (end field-values)
=>
 (printout t "error clean-up-word-facts" crlf))

(defrule clean-up-string-facts
 (declare (salience -20)) ; Added by Gary Riley
 ?f1 <- (string $?)
 (end field-values)
=>
 (retract ?f1))

(defrule error-clean-up-string-facts
 (declare (salience -30)) ; Added by Gary Riley
 (string $?)
 (end field-values)
=>
 (printout t "error clean-up-string-facts" crlf))

(defrule clean-up-delimeter-facts
 (declare (salience -20)) ; Added by Gary Riley
 ?f1 <- (delimeter $?)
 (end field-values)
=>
 (retract ?f1))

(defrule error-clean-up-delimeter-facts
 (declare (salience -30)) ; Added by Gary Riley
 (delimeter $?)
 (end field-values)
=>
 (printout t "error clean-up-delimeter-facts" crlf))

(defrule begin-wildcards-test
 (declare (salience -40)) ; Added by Gary Riley
 (end field-values)
 =>
 (assert (begin wildcards)))
 
;;; 3.2 Wildcards - Single and Multi-field

;;; In this file, single field and multiple field wildcards are employed in
;;; retracting facts during the clean up operations. In this section, single
;;; field and multi-field wildcards are used in various combinations within
;;; a single pattern. Additionally, multi-field wildcard and literal fields
;;; are combined in a single pattern. Assumption: If a wildcard performs
;;; correctly in one field, then it will perform correctly in all fields.

;;; ========= INITIAL FACTS FOR SINGLE AND MULTIPLE FIELD WILDCARDS =========


(deffacts wildcards "facts for single and multi-field"
 (data red)
 (data red green)
 (data red "green")
 (data red green green)
 (data green red)
 (data red 3.5e3))

;;; ======================= WILDCARD MATCHING =================================


(defrule single-field-wildcard "matching"
 (declare (salience 1)) ; Added by Gary Riley
 (begin wildcards)
 (data red ?)
=>
 (assert (successful "single field matching"))
 (printout t "single-field-wildcard should fire three times" crlf))

(defrule multi-field-wildcard "matching"
 (declare (salience 2)) ; Added by Gary Riley
 (begin wildcards)
 (data red $?)
=>
 (assert (successful "multi field matching"))
 (printout t "multi-field-wildcard should fire five times" crlf))

(defrule multi-single-field-wildcard "matching and variable delimeters"
 (declare (salience 3)) ; Added by Gary Riley
 (begin wildcards)                                               ;CR0001
 (data ??)                                                        ;CR0001
=>
 (assert (successful "multi single field wildcard matching"))
 (printout t "multi-single-field-wildcard should fire four times" crlf))


(defrule multi-multi-field-wildcard "matching and variable delimeters"
 (declare (salience 4)) ; Added by Gary Riley
 (begin wildcards)
 (data $? $?)
=>
 (assert (successful "multi multi field wildcard matching"))
 (printout t "multi-multi-field-wildcard should fire eighteen times" crlf))

(defrule multi-and-single-field-wildcard "matching"
 (declare (salience 5)) ; Added by Gary Riley
 (begin wildcards)
 (data $? ?)
=>
 (assert (successful "multi and single field wildcards matching"))
 (printout t "multi-and-single-field-wildcard should fire six times" crlf))

(defrule multi-field-and-literal-field "matching"                ;CR0001
 (declare (salience 6)) ; Added by Gary Riley
 (begin wildcards)
 (color $? YELLOW $?)                                            ;CR0001
=>                                                               ;CR0001
 (assert (successful "multi field and literal field"))           ;CR0001
 (printout t "multi-field-and-literal-field should fire six times" crlf))

(defrule assert-color-facts "add data"
 (successful "single field matching")
 (successful "multi field matching")
 (successful "multi multi field wildcard matching")
 (successful "multi single field wildcard matching")
 (successful "multi and single field wildcards matching")
=>
 (assert (color YELLOW blue red green))
 (assert (color YELLOW red))
 (assert (color red YELLOW))
 (assert (color YELLOW))
 (assert (color YELLOW color YELLOW)))

(defrule wildcard-clean-up
 (declare (salience -10))
 ?f1 <- (begin wildcards)
 ?f2 <- (successful "single field matching")
 ?f3 <- (successful "multi field matching")
 ?f4 <- (successful "multi single field wildcard matching")
 ?f5 <- (successful "multi multi field wildcard matching")
 ?f6 <- (successful "multi and single field wildcards matching")
 ?f7 <- (successful "multi field and literal field")
=>
 (retract ?f1 ?f2 ?f3 ?f4 ?f5 ?f6 ?f7)
 (assert (end wildcards)))

(defrule error-wildcards
 (declare (salience -30))
 (begin wildcards)
=>
 (printout t "error in wildcard matching" crlf))

(defrule color-facts-clean-up
 (declare (salience -20)) ; Added by Gary Riley
 (end wildcards)
 ?f1 <- (color $? YELLOW $?)
=>
 (retract ?f1))

(defrule error-color-facts-clean-up
 (declare (salience -30)) ; Added by Gary Riley
 (end wildcards)
 (color $? YELLOW $?)
=>
 (printout t "error in color facts clean up" crlf))

(defrule begin-variable-matching-test
 (declare (salience -40)) ; Added by Gary Riley
 (end wildcards)
 =>
 (assert (begin variable-matching)))

;;; 3.3 Variables- Single and Multi-field

;;; Single field and multi-field variables are used in various combinations in
;;; single and multiple patterns.  Assumption:  If a single or multiple field
;;; variable in a field performs correctly, then it will perform correctly in
;;; all fields.


;;; ================= SINGLE AND MULTIPLE FIELD VARIABLES ====================
;;; ========================== SINGLE PATTERN ================================

(defrule single-variable-single-pattern "matching"
 (declare (salience 1)) ; Added by Gary Riley
 (begin variable-matching)
 (data red ?x)
=>
 (assert (rule-1 ?x))
 (printout t "single-variable-single-pattern should fire three")
        (printout t " times" crlf))

(defrule multi-field-variable-single-pattern "matching"     ;DR0087
 (declare (salience 2)) ; Added by Gary Riley
 (begin variable-matching)                                  ;DR0087
 (data red $?x)                                             ;DR0087
=>                                                          ;DR0087
 (assert (rule-2 ?x))                                      ;DR0087
 (printout t "multi-field-variable-single-pattern should fire five");DR0087
 (printout t " times" crlf))                               ;DR0087

(defrule multi-single-variable-single-pattern
 (declare (salience 3)) ; Added by Gary Riley
 (begin variable-matching)
 (data ?x ?y)
=>
 (assert (rule-3 ?x ?y))
 (printout t "multi-single-variable-single-pattern should fire four")
 (printout t " times" crlf))


(defrule multi-multi-field-variable-single-pattern          ;DR0087
 (declare (salience 4)) ; Added by Gary Riley
 (begin variable-matching)                                  ;DR0087
 (data red $?x $?y)                                         ;DR0087
=>                                                          ;DR0087
 (assert (rule-4 x ?x y ?y))                              ;DR0087
 (printout t "multi-multi-field-variable-single-pattern should fire");DR0087
 (printout t " ten times" crlf))                           ;DR0087

(defrule single-and-multi-field-variable-single-pattern     ;DR0087
 (declare (salience 5)) ; Added by Gary Riley
 (begin variable-matching)                                  ;DR0087
 (data red $?x ?y)                                          ;DR0087
=>                                                          ;DR0087
 (assert (rule-5 x ?x y ?y))                               ;DR0087
 (printout t "single-and-multi-field-variable-single-pattern should");DR0087
 (printout t " fire four times" crlf))                     ;DR0087

(defrule single-pattern-variable-clean-up "Change by Gary Riley"
 (declare (salience -10))
 ?f1 <- (begin variable-matching)
 (rule-1 green)
 (rule-1 "green")
 (rule-1 3500.0)
 (rule-2)
 (rule-2 green)
 (rule-2 "green")
 (rule-2 green green)
 (rule-2 3500.0)
 (rule-3 red green)
 (rule-3 red 3500.0)
 (rule-3 green red)
 (rule-3 red "green")
 (rule-4 x 3500.0 y)
 (rule-4 x y 3500.0)
 (rule-4 x green green y)
 (rule-4 x y)
 (rule-4 x y green)
 (rule-4 x green y)
 (rule-4 x y "green")
 (rule-4 x "green" y)
 (rule-4 x y green green)
 (rule-4 x green y green)
 (rule-5 x y green)
 (rule-5 x y 3500.0)
 (rule-5 x green y green)
 (rule-5 x y "green")
=>
 (retract ?f1)
 (assert (end variable-matching)))

(defrule error-variable-matching
 (declare (salience -30)) ; Added by Gary Riley
 (begin variable-matching)
=>
 (printout t "error in variable matching" crlf))

(defrule clean-up-data-facts
 (declare (salience -20)) ; Added by Gary Riley
 (end variable-matching)
 ?f1 <- (data ? $?)
=>
 (retract ?f1))

(defrule clean-up-rule-1-facts
 (declare (salience -20)) ; Added by Gary Riley
 (end variable-matching)
 ?f1 <- (rule-1 $?)
=>
 (retract ?f1))

(defrule clean-up-rule-2-facts
 (declare (salience -20)) ; Added by Gary Riley
 (end variable-matching)
 ?f1 <- (rule-2 $?)
=>
 (retract ?f1))

(defrule clean-up-rule-3-facts
 (declare (salience -20)) ; Added by Gary Riley
 (end variable-matching)
 ?f1 <- (rule-3 $?)
=>
 (retract ?f1))

(defrule clean-up-rule-4-facts
 (declare (salience -20)) ; Added by Gary Riley
 (end variable-matching)
 ?f1 <- (rule-4 $?)
=>
 (retract ?f1))

(defrule clean-up-rule-5-facts
 (declare (salience -20)) ; Added by Gary Riley
 (end variable-matching)
 ?f1 <- (rule-5 $?)
=>
 (retract ?f1))

(defrule error-clean-up-data-rule-1-facts
 (declare (salience -30)) ; Added by Gary Riley
 (end variable-matching)
 (or (rule-1 $?)
  (rule-2 $?)
  (rule-3 $?)
  (rule-4 $?)
  (rule-5 $?))
=>
 (printout t "error in cleaning up rule facts for single patterns" crlf))

(defrule error-clean-up-data-facts
 (declare (salience -30)) ; Added by Gary Riley
 (end variable-matching)
 (data ? $?)
=>
 (printout t "error in cleaning up data facts for single patterns" crlf))
 
(defrule begin-multiple-pattern-matching-test
 (declare (salience -40)) ; Added by Gary Riley
 (end variable-matching)
 =>
 (assert (begin multiple-pattern-matching)))
 
;;; ================ SINGLE AND MULTIPLE FIELD VARIABLES ======================
;;; ========================= MULTIPLE PATTERNS ===============================


(defrule data-facts-for-multiple-pattern-matching
 (declare (salience -10))
 (begin multiple-pattern-matching)
 ?f <- (end variable-matching)
=>
 (retract ?f)
 (assert (data red))
 (assert (data green))
 (assert (data blue))
 (assert (data 10.5))
 (assert (data red green))
 (assert (data red blue))
 (assert (data purple blue))
 (assert (data purple green))
 (assert (data red blue green))
 (assert (data purple blue green))
 (assert (data blue red blue)))

(defrule single-field-variable-multiple-patterns
 (declare (salience 1)) ; Added by Gary Riley
 (begin multiple-pattern-matching)
 (data red ?x)
 (data purple ?x)
=>
 (assert (rule-1 ?x))
 (printout t "single-field-variable-multiple-patterns should fire")
 (printout t " twice" crlf))

(defrule multi-field-variable-multiple-patterns              ;DR0087
 (declare (salience 2)) ; Added by Gary Riley
 (begin multiple-pattern-matching)                           ;DR0087
 (data red $?x)                                              ;DR0087
 (data purple $?x)                                           ;DR0087
=>                                                           ;DR0087
 (assert (rule-2 ?x))                                       ;DR0087
 (printout t "multi-field-variable-multiple-patterns should fire");DR0087
 (printout t " three times" crlf))                          ;DR0087


(defrule multiple-pattern-matching-clean-up 
 (declare (salience -10))
 ?f <- (begin multiple-pattern-matching)
 (rule-1 green)
 (rule-1 blue)
 (rule-2 green)
 (rule-2 blue)
 (rule-2 blue green)
=>
 (retract ?f)
 (assert (end multiple-pattern-matching)))

(defrule error-multiple-pattern-matching
 (declare (salience -30))
 (begin multiple-pattern-matching)
=>
 (printout t "error multiple pattern variable matching" crlf))

(defrule fact-clean-up-multiple-pattern
 (declare (salience -20)) ; Added by Gary Riley
 ?f2 <- (rule-1 green)
 ?f3 <- (rule-1 blue)
 ?f4 <- (rule-2 green)
 ?f5 <- (rule-2 blue)
 ?f6 <- (rule-2 blue green)
=>
 (retract ?f2 ?f3 ?f4 ?f5 ?f6))

(defrule error-fact-clean-up-multiple-pattern
 (declare (salience -30)) ; Added by Gary Riley
 (or (rule-1 $?)
     (rule-2 $?))
=>
 (printout t "error in multiple pattern matching variable values" crlf))

(defrule begin-logical-operators-test
 (declare (salience -40)) ; Added by Gary Riley
 ?f1 <- (end multiple-pattern-matching)
 =>
 (retract ?f1)
 (assert (begin logical-operators)))
 
;;; 3.4 Constraining Fields

;;; 3.4.1 Logical Operators

;;; The three logical operators &, ~ and | are used in single and multiple
;;; patterns with or without variable assignment.  Assumption: If a logical
;;; operator performs correctly in one field of a pattern, it will correctly
;;; perform in every field.


;;; ===================== CONSTRAINING FIELDS ================================
;;; ====================== LOGICAL OPERATORS =================================


(defrule not-with-no-variable-assignment
 (declare (salience 8)) ; Added by Gary Riley
 (begin logical-operators)
 (data ~red)
=>
 (printout t "not-with-no-variable-assignment should fire three")
 (printout t " times" crlf))

(defrule or-with-no-variable-assignment
 (declare (salience 7)) ; Added by Gary Riley
 (begin logical-operators)
 (data red|blue)
=>
 (printout t "or-with-no-variable-assignment should fire twice" crlf))

(defrule not-and-with-no-variable-assignment
 (declare (salience 6)) ; Added by Gary Riley
 (begin logical-operators)
 (data ~red&~blue)
=>
 (printout t "not-and-with-no-variable-assignment should fire twice")
 (printout t " " crlf))

(defrule not-or-and-with-no-variable-assignment
 (declare (salience 5)) ; Added by Gary Riley
 (begin logical-operators)
 (data ~red&~blue|green)
=>
 (printout t "not-or-and-with-no-variable-assignment should fire")
 (printout t " twice  " crlf))

(defrule not-with-variable-assignment
 (declare (salience 4)) ; Added by Gary Riley
 (begin logical-operators)
 (data ?x&~red)
=>
 (assert (rule-1 ?x))
 (printout t "not-with-variable-assignment should fire three times")
 (printout t " " crlf))

(defrule or-with-variable-assignment
 (declare (salience 3)) ; Added by Gary Riley
 (begin logical-operators)
 (data ?x&red|blue)
=>
 (assert (rule-2 ?x))
 (printout t "or-with-variable-assignment should fire twice" crlf))

(defrule not-with-variable-assignment-multiple-patterns
 (declare (salience 2)) ; Added by Gary Riley
 (begin logical-operators)
 (data red ?x)
 (data purple ?x&~green)
=>
 (assert (rule-3 ?x))
 (printout t "not-with-variable-assignment-multiple-patterns")
 (printout t " should fire once" crlf))

(defrule or-with-variable-assignment-multiple-patterns
 (declare (salience 1)) ; Added by Gary Riley
 (begin logical-operators)
 (data red ?x)
 (data purple ?x&green|blue)
=>
 (assert (rule-4 ?x))
 (printout t "or-with-variable-assignment-multiple-patterns")
 (printout t " should fire twice" crlf))

(defrule logical-operator-clean-up
 (declare (salience -10)) ; Added by Gary Riley
 ?f <- (begin logical-operators)
 ?f1 <- (rule-1 blue)
 ?f2 <- (rule-1 green)
 ?f3 <- (rule-1 10.5)
 ?f4 <- (rule-2 red)
 ?f5 <- (rule-2 blue)
 ?f6 <- (rule-3 blue)
 ?f7 <- (rule-4 green)
 ?f8 <- (rule-4 blue)
=>
 (retract ?f ?f1 ?f2 ?f3 ?f4 ?f5 ?f6 ?f7 ?f8)
 (assert (end logical-operators)))

(defrule error-logical-operator-clean-up
 (declare (salience -30))
 (begin logical-operators)
=>
 (printout t "error in logical operators" crlf))

(defrule data-facts-clean-up-two
 (declare (salience -20)) ; Added by Gary Riley
 (end logical-operators)
 ?f <- (data $?)
=>
 (retract ?f))

(defrule error-data-facts-clean-up-two
 (declare (salience -30)) ; Changed by Gary Riley
 (end logical-operators)
 (data $?)
=>
 (printout t "error in data facts clean up two"))

(defrule error-clean-up-rule-facts-logical-operators
 (declare (salience -30)) ; Added by Gary Riley
 (end logical-operators)
 (or (rule-1 $?)
  (rule-2 $?)
  (rule-3 $?)
  (rule-4 $?))
=>
 (printout t "error in logical-operators values" crlf))
 
(defrule begin-predicate-functions-test
 (declare (salience -40)) ; Added by Gary Riley
 (end logical-operators)
 =>
 (assert (begin predicate-functions)))
 
 
;;; 3.4.2 Predicate Functions

;;; Predicate functions check to see if the value of the field meets the
;;; constraints defined in the function.  If it does, the function returns
;;; true (non-zero) and pattern matching continues.  Otherwise, it returns
;;; false (0) and the pattern fails to match.  In this section, predicate
;;; functions provided by CLIPS are tested in single patterns.


;;; ======================= CONSTRAINING FIELDS =============================
;;; ======================= PREDICATE FUNCTIONS =============================
;;; =======================   SINGLE FUNCTIONS  =============================

(defrule data-facts-for-predicate-functions
 (declare (salience -10))
 (begin predicate-functions)
 ?f <- (end logical-operators)
=>
 (retract ?f)
 (assert (data 2))
 (assert (data red))
 (assert (data 11))
 (assert (data "red"))
 (assert (data -13))
 (assert (data 6.5))
 (assert (data 0)))

(defrule predicate-function-numberp
 (declare (salience 6)) ; Added by Gary Riley
 (begin predicate-functions)
 (data ?x&:(numberp ?x))
=>
 (assert (rule-1 ?x))
 (printout t "predicate-function-numberp should fire five times")
 (printout t " " crlf))

(defrule predicate-function-evenp                     ;DR0056 CR0025
 (declare (salience 5)) ; Added by Gary Riley
 (begin predicate-functions)                          ;DR0056 CR0025
 (data ?x&:(integerp ?x)&:(evenp ?x))                 ;DR0056 CR0025
=>                                                    ;DR0056 CR0025
 (assert (rule-2 ?x))                                 ;DR0056 CR0025
 (printout t "predicate-function-evenp should fire twice" crlf));DR0056 CR0025

(defrule predicate-function-oddp                     ;DR0056 CR0025
 (declare (salience 4)) ; Added by Gary Riley
 (begin predicate-functions)                         ;DR0056 CR0025
 (data ?x&:(integerp ?x)&:(oddp ?x))                 ;DR0056 CR0025
=>                                                   ;DR0056 CR0025
 (assert (rule-3 ?x))                                ;DR0056 CR0025
 (printout t "predicate-function-oddp should fire twice" crlf));DR0056 CR0025

(defrule predicate-function-stringp
 (declare (salience 3)) ; Added by Gary Riley
 (begin predicate-functions)
 (data ?x&:(stringp ?x))
=>
 (assert (rule-4 ?x))
 (printout t "predicate-function-stringp should fire once" crlf))

(defrule predicate-function-wordp                               ;DR0059
 (declare (salience 2)) ; Added by Gary Riley
 (begin predicate-functions)                                    ;DR0059
 (data ?x&:(wordp ?x))                                          ;DR0059
=>                                                              ;DR0059
 (assert (rule-5 ?x))                                           ;DR0059
 (printout t "predicate-function-wordp should fire once" crlf));DR0059

(defrule predicate-function-integerp
 (declare (salience 1)) ; Added by Gary Riley
 (begin predicate-functions)
 (data ?x&:(numberp ?x)&:(integerp ?x))
=>
 (assert (rule-6 ?x))
 (printout t "predicate-function-integerp should fire four times" crlf))

(defrule predicate-functions-clean-up
 (declare (salience -10)) ; Added by Gary Riley
 ?f <- (begin predicate-functions)
 ?f1 <- (rule-1 2)
 ?f2 <- (rule-1 11)
 ?f3 <- (rule-1 -13)
 ?f4 <- (rule-1 0)
 ?f5 <- (rule-2 2)
 ?f6 <- (rule-2 0)
 ?f7 <- (rule-3 11)
 ?f8 <- (rule-3 -13)
 ?f9 <- (rule-4 "red")
 ?f10 <- (rule-5 red)
 ?f11 <- (rule-1 6.5)
 ?f12 <- (rule-6 2)
 ?f13 <- (rule-6 11)
 ?f14 <- (rule-6 -13)
 ?f15 <- (rule-6 0)
=>
 (retract ?f ?f1 ?f2 ?f3 ?f4 ?f5 ?f6 ?f7 ?f8 ?f9 ?f10 ?f11 ?f12 ?f13 ?f14 ?f15)
 (assert (end predicate-functions)))

(defrule error-prediate-functions-clean-up
 (declare (salience -30)) ; added by Gary Riley
 (begin predicate-functions)
=>
 (printout t "error in predicate functions" crlf))

(defrule data-facts-clean-up-three
 (declare (salience -20)) ; Added by Gary Riley
 (end predicate-functions)
 ?f <- (data $?)
=>
 (retract ?f))

(defrule error-clean-up-rules-for-predicate-functions
 (declare (salience -30)) ; Added by Gary Riley
 (end predicate-functions)
 (or (rule-1 ?)
  (rule-2 ?)
  (rule-3 ?)
  (rule-4 ?)
  (rule-5 ?)
  (rule-6 ?))
=>
 (printout t "error in predicate functions values" crlf))
 
(defrule begin-constraining-functions-test
 (declare (salience -40)) ; Added by Gary Riley
 (end predicate-functions)
 =>
 (assert (begin constraining-functions)))
 
 
;;; 3.4.3 Constraining fields through pattern expansion

;;; The equals (=) operator permits an external functions to be called from
;;; the inside of a pattern.  The return value of the external function
;;; constrains the the value of a field.  In this section, external
;;; functions provided with CLIPS are tested.


;;; ===================== CONSTRAINING FUNCTIONS ============================
;;; ======================= EXTERNAL FUNCTIONS ==============================


(defrule point-facts-for-constraining-functions
 (declare (salience -10))
 (begin constraining-functions)
 ?f <- (end predicate-functions)
=>
 (retract ?f)
 (assert (point A 0 3))
 (assert (point B 4 6))
 (assert (point C -6 3))
 (assert (point D 4 -12)))

(defrule external-functions-logical-operators
 (declare (salience 6)) ; Added by Gary Riley
 (begin constraining-functions) ; Added by Gary Riley
 (point ?p1 ?x1 ?)
 (point ?p2 ?x2&=(+ ?x1 -10)|=(- ?x1 4) ?)
=>
 (assert (rule-1 ?p1 ?p2 ?x1 ?x2))
 (printout t "external-functions-logical-operators should fire four")
 (printout t " times" crlf))


(defrule constraining-functions-vertical
 (declare (salience 5)) ; Added by Gary Riley
 (begin constraining-functions) ; Added by Gary Riley
 (point ?p1 ?x1 ?)
 (point ?p2&~?p1 ?x2&:(eq ?x1 ?x2) ?)
=>
 (assert (rule-2 ?p1 ?p2 vertical))
 (printout t "constraining-functions-vertical should fire twice")
 (printout t " " crlf))

(defrule constraining-functions-not-vertical
 (declare (salience 4)) ; Added by Gary Riley
 (begin constraining-functions) ; Added by Gary Riley
 (point ?p1 ?x1 ?)
 (point ?p2&~?p1 ?x2&:(neq ?x1 ?x2) ?)
=>
 (assert (rule-3 ?p1 ?p2 not-vertical))
 (printout t "constraining-functions-not-vertical should fire")
 (printout t " ten times" crlf))

;;; 3.5 Test to Constrain Variables
;;;
;;; Any external function may be embedded within a test operation. All defined
;;; functions, user or CLIPS functions use the prefix notation.


;;; ======================= CONSTRAINING FUNCTIONS ===========================
;;; ============================ TEST FUNCTION ===============================


(defrule slope-of-line-negative-test-function
 (declare (salience 3)) ; Added by Gary Riley
 (begin constraining-functions) ; Added by Gary Riley
 (point ?p1 ?x1 ?y1)
 (point ?p2 ?x2 ?y2)
 (test (and ( neq 0 ( - ?x2 ?x1))
  (or ( neq ?x1 ?x2)( neq ?y1 ?y2))
  ( > 0 ( / ( - ?y2 ?y1) ( - ?x2 ?x1)))))
=>
 (assert (rule-4 ?p1 ?p2 "+" slope))
 (printout t "slope-of-line-negative-test-function should")
 (printout t " fire four times" crlf))

(defrule slope-of-line-positive-test-function
 (declare (salience 2)) ; Added by Gary Riley
 (begin constraining-functions) ; Added by Gary Riley
 (point ?p1 ?x1 ?y1)
 (point ?p2 ?x2 ?y2)
 (test (and ( neq 0 ( - ?x2 ?x1))
  (or ( neq ?x1 ?x2)( neq ?y1 ?y2))
  ( < 0 ( / ( - ?y2 ?y1) ( - ?x2 ?x1)))))
=>
 (assert (rule-5 ?p1 ?p2 "-" slope))
 (printout t "slope-of-line-positive-test-function")
 (printout t " fire four times" crlf))

(defrule fact-address-test-function                                  ;CR0023
 (declare (salience 1)) ; Added by Gary Riley
 (begin constraining-functions) ; Added by Gary Riley
  ?f1 <- (point ~B $?)                                               ;CR0023
  ?f2 <- (point ~C $?)                                               ;CR0023
  (test (neq ?f1 ?f2))                                               ;CR0023
=>                                                                   ;CR0023
  (printout t "fact-address-test-function should fire seven times" crlf));CR0023

(defrule constraining-functions-clean-up
 (declare (salience -10)) ; Added by Gary Riley
 ?f <- (begin constraining-functions)
 ?f1 <- (rule-1 D A 4 0)
 ?f2 <- (rule-1 D C 4 -6)
 ?f3 <- (rule-1 B C 4 -6)
 ?f4 <- (rule-1 B A 4 0)
 ?f5 <- (rule-2 D B vertical)
 ?f6 <- (rule-2 B D vertical)
 ?f7 <- (rule-3 D A not-vertical)
 ?f8 <- (rule-3 A D not-vertical)
 ?f9 <- (rule-3 B A not-vertical)
 ?f10 <- (rule-3 D C not-vertical)
 ?f11 <- (rule-3 C D not-vertical)
 ?f12 <- (rule-3 C A not-vertical)
 ?f13 <- (rule-3 A C not-vertical)
 ?f14 <- (rule-3 C B not-vertical)
 ?f15 <- (rule-3 B C not-vertical)
 ?f16 <- (rule-3 A B not-vertical)
 ?f17 <- (rule-5 B C "-" slope)
 ?f18 <- (rule-5 C B "-" slope)
 ?f19 <- (rule-5 B A "-" slope)
 ?f20 <- (rule-5 A B "-" slope)
 ?f21 <- (rule-4 A D "+" slope)
 ?f22 <- (rule-4 D A "+" slope)
 ?f23 <- (rule-4 C D "+" slope)
 ?f24 <- (rule-4 D C "+" slope)
=>
 (retract ?f ?f1 ?f2 ?f3 ?f4 ?f5 ?f6 ?f7 ?f8 ?f9 ?f10 ?f11 ?f12 ?f13)
 (retract ?f14 ?f15 ?f16 ?f17 ?f18 ?f19 ?f20 ?f21 ?f22 ?f23 ?f24)
 (assert (end constraining-functions)))


(defrule error-constraining-functions-clean-up
 (declare (salience -30)) ; Added by Gary Riley 
 (begin constraining-functions)
=>
 (printout t "error in constraining functions" crlf))

(defrule point-facts-clean-up
 (declare (salience -20)) ; Added by Gary Riley
 (end constraining-functions)
 ?f <- (point $?)
=>
 (retract ?f))

(defrule error-clean-up-rules-for-constraining-functions
 (declare (salience -30)) ; Added by Gary Riley
 (end constraining-functions)
 (or (rule-1 $?)
  (rule-2 $?)
  (rule-3 $?)
  (rule-4 $?))
=>
 (printout t "error in constraining functions values" crlf))

(defrule begin-constraining-patterns-test
 (declare (salience -40)) ; Added by Gary Riley
 (end constraining-functions)
 =>
 (assert (begin constraining-patterns)))

 
;;; 3.6 Constraining Patterns

;;; Logcial pattern blocks allow patterns to be combined using inclusive OR,
;;; explicit AND, and negation NOT.  Single, multiple and nested inclusive ORs
;;; are tested.  OR with nested ANDs is also tested in this section.


;;; ====================== CONSTRAINING PATTERNS ============================
;;; =========================== OR AND NOT ==================================

(defrule facts-for-constraining-patterns
 (declare (salience -10))
 (begin constraining-patterns)
 ?f <- (end constraining-functions)
=>
 (retract ?f)
 (assert (letter a))
 (assert (letter b))
 (assert (letter c))
 (assert (letter d))
 (assert (letter e))
 (assert (block a))
 (assert (block b))
 (assert (block c))
 (assert (pattern A))
 (assert (pattern B))
 (assert (part A))
 (assert (part B)))

(defrule or-not-nested-constraining-patterns
 (declare (salience 4)) ; Added by Gary Riley
 (begin constraining-patterns)
 (or (letter a)
  (letter b)
  (letter c))
=>
 (assert (successful "or not nested constraining patterns"))
 (printout t "or-not-nested-constraining-patterns should fire three")
 (printout t " times" crlf))

(defrule multiple-ors-constraining-patterns
 (declare (salience 3)) ; Added by Gary Riley
 (begin constraining-patterns)
 (or (letter a)
  (letter b))
 (or (block a)
  (block b))
=>
 (assert (successful "multiple ors constraining patterns"))
 (printout t "multiple-ors-constraining-patterns should fire four")
 (printout t " times" crlf))

(defrule nested-ors-constraining-patterns
 (declare (salience 2)) ; Added by Gary Riley
 (begin constraining-patterns)
 (or
   (or (letter a)
   (letter b))
  (or (block a)
   (block b)))
=>
 (assert (successful "nested ors constraining patterns"))
 (printout t "nested-ors-constraining-patterns should fire")
 (printout t " four times" crlf))

(defrule nested-ands-or-not-constraining-patterns
 (declare (salience 1)) ; Added by Gary Riley
 (begin constraining-patterns)
 (or (and (letter ?x)
   (block ?x))
  (and (pattern ?x)
   (part ?x)))
 (letter ?y)
 (not (block ?y))
=>
 (assert (rule-1 ?x ?x only-letter ?y))
 (printout t "nested-ands-or-not-constraining-patterns should fire")
 (printout t " ten times" crlf))

(defrule constraining-patterns-clean-up
 (declare (salience -10))
 ?f <- (begin constraining-patterns)
 ?f1 <- (successful "multiple ors constraining patterns")
 ?f2 <- (successful "nested ors constraining patterns")
 ?f3 <- (successful "or not nested constraining patterns")
 (rule-1 B B only-letter d)
 (rule-1 B B only-letter e)
 (rule-1 A A only-letter d)
 (rule-1 A A only-letter e)
 (rule-1 c c only-letter d)
 (rule-1 c c only-letter e)
 (rule-1 b b only-letter d)
 (rule-1 b b only-letter e)
 (rule-1 a a only-letter d)
 (rule-1 a a only-letter e)
=>
 (retract ?f ?f1 ?f2 ?f3)
 (assert (end constraining-patterns)))


(defrule error-constraining-patterns
 (declare (salience -30))
 (begin constraining-patterns)
=>
 (printout t "error in constraining patterns" crlf))

(defrule clean-up-letter-facts
 (declare (salience -20)) ; Added by Gary Riley
 (end constraining-patterns)
 ?f <- (letter ?)
=>
 (retract ?f))

(defrule clean-up-block-facts
 (declare (salience -20)) ; Added by Gary Riley
 (end constraining-patterns)
 ?f <- (block ?)
=>
 (retract ?f))

(defrule clean-up-pattern-facts
 (declare (salience -20)) ; Added by Gary Riley
 (end constraining-patterns)
 ?f <- (pattern ?)
=>
 (retract ?f))

(defrule clean-up-part-facts
 (declare (salience -20)) ; Added by Gary Riley
 (end constraining-patterns)
 ?f <- (part ?)
=>
 (retract ?f))

(defrule clean-up-rule-1-facts-two
 (declare (salience -20)) ; Added by Gary Riley
 (end constraining-patterns)
 ?f <- (rule-1 $?)
=>
 (retract ?f))

(defrule error-clean-up-constraining-patterns-facts
 (declare (salience -30)) ; Added by Gary Riley
 (end constraining-patterns)
 (or (letter ?)
  (block ?)
  (pattern ?)
  (part ?)
  (rule-1 $?))
=>
 (printout t "error in cleaning up constraining patterns facts" crlf))


(defrule begin-rule-properties-test
 (declare (salience -40)) ; Added by Gary Riley
 (end constraining-patterns)
 =>
 (assert (begin rule-properties)))
 
;;; 3.7 Pattern bindings

;;; A variable can be bound to an entire fact.  Pattern bindings are tested in
;;; all clean up rules with different patterns containing a variable number of
;;; fields and constraints.

;;; 3.8 Declaring rule properties

;;; Properties or characteristics of a rule may be defined in the declare
;;; construct.  All declare statements must appear before the first pattern on
;;; the LHS.

;;; 3.8.1 Assigning rule priority

;;; A salience statement allows the assignment of a priority to a rule.  In
;;; this file salience, in conjunction with control facts, is employed to phase
;;; from one testing sequence to another.  Additionally, sets of clean up rules
;;; are controlled with salience.  Therefore only maximum and minimum salience
;;; values are tested in this section.


;;; ========================== RULE PROPERTIES =================================
;;; ========================== DECLARE SALIENCE ================================


(defrule salience-max-value
 (declare (salience 10000))
 (initial-fact)
=>
 (printout t crlf "TESTING CLIPS - BASIC PROGRAMMING GUIDE" crlf))

(defrule salience-positive
 (declare (salience 100))
 (initial-fact)
=>
 (printout t "common capabilities such as:" crlf
              "assert, retract, salience, fprintout" crlf
              "are exercised all through the test" crlf))

(defrule salience-min-value
 (declare (salience -10000))
 ?f <- (initial-fact)
=>
 (retract ?f)
 (printout t "list of facts that were not cleaned up, error if any" crlf)
 (facts)
 (printout t "file text.tmp may be deleted" crlf
              "CLIPS TESTING COMPLETED" crlf crlf))

;;; 4.0 Actions

;;; 4.1 Basic actions


;;; ========================= ACTIONS FUNCTIONS ==============================
;;; ===================== ASSERTS RETRACT STR_ASSERT =========================
;;; ======================== BIND HALT OPEN CLOSE ============================

;;; 4.1.1 Creating new facts

;;; The assert action permits a fact to be added to the fact-list.  Facts
;;; containing bound single and multiple field variables, in addition to literal
;;; values, are asserted during the testing beginning in section 3.3.  Only
;;; assert actions containing function calls will be tested in this section.


(defrule assert-facts-for-actions
 (declare (salience -10))
 ?f1 <- (begin rule-properties)
 ?f2 <- (end constraining-patterns)
=>
 (retract ?f1 ?f2)
 (assert (begin action-assert-retract))
 (assert (data numbers 2.75e3 -53.7 6.25e2))
 (assert (data string "Computer Sciences Corporation"))
 (assert (charles specialchar!@#$%^*_+{}:>`-=\[]'./))
 (assert (charles "specialchar~!@#$%^&*()_+|{}:\"<>?`-=\\[];',./"))
 (assert (chuck delimeter ab<cd"a"x
y)))

(defrule assert-facts-using-external-functions-1                 ;DR0023
 (declare (salience 1))
 (begin action-assert-retract)                                   ;DR0023
 (data numbers ?num-1 ?num-2 ?num-3)                             ;DR0023
=>                                                               ;DR0023
 (assert (rule-1 numbers =(/ ?num-1 ?num-3) =(* ?num-1 ?num-2)   ;DR0023
    =(max ?num-1 ?num-2 ?num-3))))                               ;DR0023

(defrule assert-facts-using-external-functions-2
 (declare (salience 3))
 (begin action-assert-retract)
=>
 (printout t "Enter the number 7: " crlf)
 (assert (rule-2 number =(read))))

;;; 4.1.2 Removing Facts From The Fact-list

;;; The retract actions allow you to remove facts from the fact-list.  All
;;; clean up rules retract one or more facts which have been bound to a fact-
;;; variable on the LHS.  Only the retraction of a fact by fact-number is tested
;;; in this section.

(defrule retract-fact-by-fact-number
 (declare (salience 2))
 (begin action-assert-retract)
=>
 (retract 31 44))
;;; f-31  (end field-values)
;;; f-44  (end wildcards)

;;; 4.1.3 Asserting A String

;;; The assert-string takes a single string and breaks it into separate fields
;;; prior to asserting the fact.  String-assert in tested in asserting
;;; with literal string arguments and then with a variable argument in
;;; combination with the readline function.

(defrule assert-string-of-facts
 (declare (salience 4))
 (begin action-assert-retract)
=>
 (assert-string "(rule-3 Steve Mueller)")
 (assert-string "(rule-3 numbers 7 3.5 -4.5e4)")
 (assert-string "(rule-3 string\"Ann\"\"Baker\")")
 (assert-string "(rule-3 delimiters a<b c d e f)")) ; Changed by Gary Riley

(defrule clean-up-assert-retract
 (declare (salience -10)) ; Added by Gary Riley
 ?f <- (begin action-assert-retract)
 (not (end field-values))
 (not (end wildcards))
 (rule-1 numbers 4.4 -147675.0 2.75e3) ; Changed by Gary Riley
 (rule-2 number 7)
 (rule-3 Steve Mueller)
 (rule-3 numbers 7 3.5 -4.5e4)
 (rule-3 string "Ann" "Baker")
 (rule-3 delimiters a <b c d e f)        ;CR0001 Change by Gary Riley
 ?f1 <- (charles specialchar!@#$%^*_+{}:>`-=\[]'./)
 ?f2 <- (charles "specialchar~!@#$%^&*()_+|{}:\"<>?`-=\\[];',./")
 ?f3 <- (chuck delimeter ab <cd "a" x y)
=>
 (retract ?f ?f1 ?f2 ?f3)
 (printout t "retract by fact number test successful" crlf
  "assert using external functions and assert-string tests successful" crlf)
 (assert (end action-assert-retract)))

(defrule error-assert-retract
 (declare (salience -30))
 (begin action-assert-retract)
=>
 (printout t "error in action assert retract values" crlf))

(defrule error-asserting-delimeters
 (declare (salience -30))
 (end action-assert-retract)
 ?f <- (chuck delimeter $?x)
=>
 (retract ?f)
 (printout t "error in asserting delimeter test" crlf))

(defrule error-asserting-specialchars
 (declare (salience -30))
 (end action-assert-retract)
 ?f <- (charles $?x)
=>
 (retract ?f)
 (printout t "error in asserting special characters" crlf))

(defrule clean-up-rule-1-action-facts
 (declare (salience -20)) ; Added by Gary Riley
 (not (begin action-functions))
 (end action-assert-retract)
 ?f <- (rule-1 $?)
=>
 (retract ?f))

(defrule error-clean-up-rule-1-action-facts
 (declare (salience -30))
 (not (begin action-functions))
 (end action-assert-retract)
 (rule-1 $?)
=>
 (printout t "error cleaning up rule 1 action facts" crlf))

(defrule clean-up-rule-2-action-facts
 (declare (salience -20)) ; Added by Gary Riley
 (end action-assert-retract)
 (not (begin action-functions))
 ?f <- (rule-2 $?)
=>
 (retract ?f))

(defrule error-clean-up-rule-2-action-facts
 (declare (salience -30))
 (end action-assert-retract)
 (rule-2 $?)
 (not (begin action-functions))
=>
 (printout t "error cleaning up rule 2 action facts" crlf))


(defrule clean-up-rule-3-action-facts
 (declare (salience -20)) ; Added by Gary Riley
 (not (begin action-functions))
 (end action-assert-retract)
 ?f <- (rule-3 $?)
=>
 (retract ?f))

(defrule error-clean-up-rule-3-action-facts
 (declare (salience -30))
 (not (begin action-functions))
 (end action-assert-retract)
 (rule-3 $?)
=>
 (printout t "error cleaning up rule 3 action facts" crlf))

(defrule clean-up-action-data-facts
 (declare (salience -20)) ; Added by Gary Riley
 (not (begin action-functions))
 (end action-assert-retract)
 ?f <- (data $?)
=>
 (retract ?f))

(defrule error-cleaning-up-action-data-facts
 (declare (salience -30))
 (not (begin action-functions))
 (end action-assert-retract)
 (data $?)
=>
 (printout t "error cleaning up action data facts" crlf))


(defrule begin-action-functions-test
 (declare (salience -40)) ; Added by Gary Riley
 (end action-assert-retract)
 =>
 (assert (begin action-functions)))
 
;;; 4.3 The CLIPS I/O System
;;;
;;; The open function allow a user to open a file form the RHS and attach a
;;; logical name to it.  A write only file is established at the beginning
;;; of this file for procedural messages.  In this section a read only file
;;; is employed to test readline function.  fprintout, read, format, open and 
;;; close are tested in the filetst program.  fprintout is used extensively
;;; throughout this file.


;;; ============================ I/O SYSTEM ==============================
;;; ================= READLINE OPEN CLOSE FORMAT fprintout ===============


(defrule openw-fprintout-close-openr-test-file-for-action-function-facts
 (declare (salience -10))
 (begin action-functions)
 ?f <- (end action-assert-retract)
=>
 (retract ?f)
 (open "Temp//text.tmp" build-text "w")
 (printout build-text "data Computer Sciences Corporation" crlf
                       "data 16511 Space Center Blvd" crlf
                       "data Houston, Tx 77058" crlf ; Changed by Gary Riley
                       "EOF" crlf)
 (close build-text)  
 (open "Temp//text.tmp" text "r")
 (assert (open text-file)))

(defrule read-line-from-test-file
 (declare (salience 3))
 (begin action-functions)
 (open text-file)
=>
 (bind ?text-string (+ 1 2))
 (while (neq ?text-string "EOF")
  (bind ?text-string (readline text))
  (assert-string (str-cat "(" ?text-string ")")))
 (close text)
 (assert (close text-file)))

(defrule bind-function
 (declare (salience 2))
 (begin action-functions)
 (length ?x)
=>
 (bind ?y (+ ?x 10))
 (assert (new-length ?y)))

(defrule halt-function
 (declare (salience 1))
 (begin action-functions)
 (new-length 13)
 (new-length 14)
=>
 (printout t "testing halt function" crlf)
 (printout t "enter the (run) commmand to continue" crlf crlf)
 (halt))

;;; 4.2 Multifield Functions


;;; ======================== MULTIFIELD FUNCTIONS ============================
;;; ====================== LENGHT MEMBER NTH SUBSET ============================


(defrule multi-field-field-manipulation-tests                 ;CR0008
 (declare (salience 4))
 (begin action-functions)
  ?f <- (multifield $?mult1)                                  ;CR0008
=>                                                            ;CR0008
 (retract ?f)                                                 ;CR0008
 (bind ?mult2 (delete$ ?mult1 2 2))                         ;CR0008
 (assert (multifield2 ?mult2))                               ;CR0008
 (bind ?mult3 (create$ ?mult2 "four" 5))      ;CR0008
 (assert (multifield3 ?mult3))                            ;CR0008
 (bind ?mult3.5 (create$ ?mult3 (create$ cww)  a (create$) b));CR0014
 (assert (multifield3 ?mult3.5))                                         ;CR0008 CR0014 
 (bind ?mult4 (str-explode "1 two"))             ;CR0008 DR0093
 (assert (multifield4 ?mult4))                                           ;CR0008 DR0093
 (bind ?string (str-implode ?mult4))                         ;CR0008
 (assert (multifield4 ?string))                                           ;CR0008
 (assert (multifield5 =(subseq$ ?mult3.5 3 33))))

(defrule length-nth-member-action-functions
 (declare (salience 5))
 (begin action-functions)
 (close text-file)
 (data $?x)
=>
 (assert (length =(length ?x)))
 (assert (second-element =(nth 2 ?x)))
 (assert (member-center =(member Center ?x)))
 (assert (multifield one two three)))

(defrule clean-up-action-functions
 (declare (salience -10))
 ?f <- (begin action-functions)
 ?f1 <- (open text-file)
 ?f2 <- (close text-file)
 ?f3 <- (EOF)
 ?f4 <- (length 3)
 ?f5 <- (length 4)
 ?f6 <- (new-length 13)
 ?f7 <- (new-length 14)
 ?f8 <- (second-element Sciences)
 ?f9 <- (second-element Space)
 ?f10 <- (second-element Tx)
 ?f11 <- (member-center FALSE) ; Changed by Gary Riley
 ?f12 <- (member-center 3)
 ?f13 <- (data Computer $?)
 ?f14 <- (data 16511 $?)
 ?f15 <- (data Houston, $?)
 ?f16 <- (multifield2 one three)                               ;CR0008
 ?f17 <- (multifield3 one three "four" 5)                      ;CR0008
 ?f18 <- (multifield3 one three "four" 5 cww a b)              ;CR0008 CR0014
 ?f19 <- (multifield4 1 two)                                   ;CR0008
 ?f20 <- (multifield4 "1 two")                                 ;CR0008
 ?f21 <- (multifield5 "four" 5 cww a b)
=>
 (retract ?f ?f1 ?f2 ?f3 ?f4 ?f5 ?f6 ?f7 ?f8 ?f9 ?f10 ?f11 ?f12 ?f13 ?f14 ?f15
          ?f16 ?f17 ?f18 ?f19 ?f20 ?f21)
 (printout t "open, close, readline, bind and assert-string tests successful" crlf
               "length, member nth and multifield tests successful" crlf)
 (assert (end action-functions)))

(defrule error-cleaning-up-action-functions
 (declare (salience -30))
 (begin action-functions)
=>
 (printout t "error in cleaning up action functions" crlf))

(defrule begin-additional-action-functions-test
 (declare (salience -40)) ; Added by Gary Riley
 (end action-functions)
 =>
 (assert (begin additional-action-functions)))
 
(defrule error-clean-up-action-functions-data
 (declare (salience -30))
 (end action-functions)
 (data $?x)
=>
 (printout t "error in cleaning up action function data" crlf))

;;; ======================== MULTIFIELD FUNCTIONS ============================
;;; ====================== LENGHT MEMBER NTH SUBSET ============================

(defrule subset-function-data
 (declare (salience -10))
 (begin additional-action-functions)
 ?f <- (end action-functions)
 (not (data $?))
=>
 (retract ?f)
 (assert (data-0 apple orange grape apple pear)
         (data-1 apple grape orange apple)
         (data-2 orange orange)))


(defrule subset-function-1
 (declare (salience 1))
 (begin additional-action-functions)
 (data-0 $?x $?y)
 (data-1 $?z)
=>
 (if (subset ?x ?z)
  then
   (assert (set ?x is-a-subset-of ?z))
  else
   (assert (set ?x is-not-a-subset-of ?z))))

(defrule subset-function-2
 (declare (salience 2))
 (begin additional-action-functions)
 (data-2 $?x $?y)
 (data-1 $?z)
=>
 (if (subset ?x ?z)
  then
   (assert (set ?x is-a-subset-of ?z))
  else
   (assert (set ?x is-not-a-subset-of ?z))))

;;; 4.4 Math Functions

;;; Math functions are tested in the mathtst file.

;;; 4.5 Additional Functions

;;; The system function is tested manually at the top level in the HP9000, VAX
;;; and PC versions.


;;; ======================== ADDITIONAL FUNCTIONS ============================
;;; ============= GENSYM SETGEN IF...THEN...ELSE WHILE STR_CAT ==============



(defrule str-cat-function-data
 (declare (salience 3))
 (begin additional-action-functions)
=>
 (assert (string "This is the first string"))
 (assert (string "This is the second string")))

(defrule str-cat-function
 (declare (salience 4))
 (begin additional-action-functions)
 (string ?x)
 (string ?y&~?x)
=>
 (bind ?string (str-cat ?x ?y))
 (assert (new-string ?string)))



(defrule str-cat-1
 (declare (salience 5))
 (begin additional-action-functions)
=>
 (bind ?string (str-cat "This is string " one))
 (assert (new-string ?string)))


(defrule str-cat-2
 (declare (salience 6))
 (begin additional-action-functions)
=>
 (bind ?string (str-cat "This is string " 1.0))
 (assert (new-string ?string)))


(defrule str-cat-3
 (declare (salience 71))
 (begin additional-action-functions)
=>
 (bind ?string (str-cat This one))
 (assert (new-string ?string)))


(defrule str-cat-4
 (declare (salience 8))
 (begin additional-action-functions)
=>
 (bind ?string (str-cat This  1.0))
 (assert (new-string ?string)))


(defrule str-cat-5
 (declare (salience 9))
 (begin additional-action-functions)
=>
 (bind ?string (str-cat 5.4e3  1.0))
 (assert (new-string ?string)))

(defrule sub-string
 (declare (salience 10))
 (begin additional-action-functions)
 (string ?x)
=>
 (assert (new-string =(sub-string 13 18 ?x))))

(defrule str-index
 (declare (salience 11))
 (begin additional-action-functions)
 (string ?x)
=>
 (assert (new-string =(str-index "second" ?x))))
 

(defrule gensym-with-setgen
 (declare (salience 13))
 (begin additional-action-functions)
=>
 (bind ?count 5)                                           ;CR0005
 (setgen 10)                                               ;CR0005
 (while (> ?count 0) do                                    ;CR0005
  (assert (data-id =(gensym)))                             ;CR0005
  (bind ?count (- ?count 1))))                             ;CR0005


(defrule gensym-without-setgen
 (declare (salience 12))  ; Added by Gary Riley
 (begin additional-action-functions)
=>
 (bind ?count 5)
 (while (> ?count 0)
  (assert (data-id =(gensym)))
  (bind ?count (- ?count 1))))

(defrule clean-up-additional-action-functions
 (declare (salience -10))
 ?f <- (begin additional-action-functions)
 ?f1 <- (set is-a-subset-of apple grape orange apple)
 ?f2 <- (set apple is-a-subset-of apple grape orange apple)
 ?f3 <- (set apple orange is-a-subset-of apple grape orange apple)
 ?f4 <- (set apple orange grape is-a-subset-of apple grape orange apple)
 ?f22 <- (set apple orange grape apple is-a-subset-of apple grape orange apple)
 ?f23 <- (set apple orange grape apple pear is-not-a-subset-of apple grape orange 
          apple)
 ?f24 <- (set orange is-a-subset-of apple grape orange apple)
 ?f25 <- (set orange orange is-a-subset-of apple grape orange apple)
 ?f5 <- (new-string "This is the second stringThis is the first string")
 ?f6 <- (new-string "This is the first stringThis is the second string")
 ?f7 <- (new-string "This is string one")
 ?f8 <- (new-string "This is string 1.0")   ; Changed by Gary Riley
 ?f9 <- (new-string "Thisone")
 ?f10 <- (new-string "This1.0")             ; Changed by Gary Riley
 ?f11 <- (new-string "5400.01.0")           ; Changed by Gary Riley
 ?f26 <- (new-string "first ")
 ?f27 <- (new-string "second")
 ?f28 <- (new-string FALSE)                 ; Changed by Gary Riley
 ?f29 <- (new-string 13) 
 ?f12 <- (data-id gen15)
 ?f13 <- (data-id gen16)
 ?f14 <- (data-id gen17)
 ?f15 <- (data-id gen18)
 ?f16 <- (data-id gen19)
 ?f17 <- (data-id gen10)
 ?f18 <- (data-id gen11)
 ?f19 <- (data-id gen12)
 ?f20 <- (data-id gen13)
 ?f21 <- (data-id gen14)
=>
 (retract ?f ?f1 ?f2 ?f3 ?f4 ?f5 ?f6 ?f7 ?f8 ?f9 ?f10 ?f11 ?f12 ?f13)
 (retract ?f14 ?f15 ?f16 ?f17 ?f18 ?f19 ?f20 ?f21 ?f22 ?f23 ?f24 ?f25)
 (retract ?f26 ?f27 ?f28 ?f29)
 (printout t "subset, str-cat, str-index, sub_string, setgen, gensym, " crlf 
              "if then else and while tests successful" crlf)
 (assert (end additional-action-functions)))

(defrule error-cleaning-up-additional-action-functions
 (declare (salience -30))
 (begin additional-action-functions)
=>
 (printout t "error in cleaning up additional action functions" crlf))

(defrule clean-up-additional-action-functions-data
 (declare (salience -20)) ; Added by Gary Riley
 (end additional-action-functions)
 ?f <- (data-0 $?x)
 ?f1 <- (data-1 $?y)
 ?f2 <- (data-2 $?z)
=>
 (retract ?f ?f1 ?f2))


(defrule clean-up-addition-action-functions-string
 (declare (salience -20)) ; Added by Gary Riley
 (end additional-action-functions)
 ?f <- (string $?x)
=>
 (retract ?f))

(defrule error-clean-up-additional-action-functions-data
 (declare (salience -30))
 (end additional-action-functions)
 (or (data-0 $?x)
     (data-1 $?x)
     (data-2 $?x)
     (string $?x))
=>
 (printout t "error in cleaning up additional action function data" crlf))
