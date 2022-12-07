(mgl-pax:define-package :aoc.parsers
  (:use :cl :mgl-pax :esrap))

(in-package :aoc.parsers)

(defsection @aoc.parsers (:title "Parsing Utilities")
  (letter dislocated)
  (digit dislocated)
  (whitespace dislocated))

(defrule letter (or (character-ranges (#\a #\z))
                    (character-ranges (#\A #\Z))))

(defrule digit (character-ranges (#\0 #\9)))

(defrule integer (+ digit)
  (:text t)
  (:function parse-integer))

(defrule whitespace (+ (or #\Space #\Tab #\Newline)))
