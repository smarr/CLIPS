;;;************************************************************
;;; DEFGLOBAL ERROR CHECKING
;;;
;;; This file tests a number of common errors which can be
;;; made with defglobals.
;;;
;;;************************************************************

(defglobal ?*x*)
(defglobal ?x)
(defglobal =)
(defglobal ?*x* =)
(defglobal ?*x* = (+ a b))
(defglobal ?*x* = ?*r*)
(defglobal ?*x* = ?x)

(defglobal ?*y* = 3 ?*x*)
(defglobal ?*z* = (create$ a b c) ?x)
(defglobal ?*w* = 5 =)
(defglobal ?*q* = (create$ d e f) ?*x* =)

(defglobal ?*y* = (create$ g h) ?*x* = (+ a b))
(defglobal ?*z* = 8 ?*x* = ?*r*)
(defglobal ?*w* = 4 ?*x* = ?x)
(defglobal ?*q* = (create$ i j k) ?*x* = ?x)