(mgl-pax:define-package :aoc.2022.04
  (:nicknames :2022.04)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :cl-ppcre #:register-groups-bind))

(in-package :2022.04)

(defsummary (:title "Camp Cleanup")
  "**Part 1** - "

  "**Part 2** - ")

(defun parse-assignment (assignment)
  (register-groups-bind ((#'parse-integer a1 a2 b1 b2))
      ("(\\d+)-(\\d+),(\\d+)-(\\d+)" assignment)
    (list a1 a2 b1 b2)))

(defun subset? (assignment)
  (destructuring-bind (a1 a2 b1 b2) assignment
    (or (<= b1 a1 a2 b2)
        (<= a1 b1 b2 a2))))

(defun build-data (&optional input)
  (read-day-input #'parse-assignment :input input))

(defun part-1 (&optional (data (build-data)))
  (count-if #'subset? data))

(defun overlap? (assignment)
  (destructuring-bind (a1 a2 b1 b2) assignment
    (or (<= a1 b1 b2 a2)
        (<= b1 a1 a2 b2)
        (<= a1 b1 a2 b2)
        (<= b1 a1 b2 a2))))

(defun part-2 (&optional (data (build-data)))
  (count-if #'overlap? data))
