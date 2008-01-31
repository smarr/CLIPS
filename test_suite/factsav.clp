(defmodule MAIN (export ?ALL))
  
(deftemplate MAIN::A (slot x))

(deftemplate MAIN::B (slot x))

(defmodule BAR (import MAIN deftemplate A B)
               (export deftemplate E))
               
(deftemplate BAR::C (slot x))

(deftemplate BAR::D (slot x))
   
(deftemplate BAR::E (slot x))

(defmodule WOZ (import BAR deftemplate E)
               (export deftemplate F))
               
(deftemplate WOZ::G (slot x))

(deftemplate WOZ::F (slot x))

(defmodule FOO (import BAR ?ALL)
               (import WOZ ?ALL)
               (import MAIN ?ALL))