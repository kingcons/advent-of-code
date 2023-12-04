(mgl-pax:define-package :aoc.2023.04
  (:nicknames :2023.04)
  (:use :cl :mgl-pax :aoc.util))

(in-package :2023.04)

(defsummary (:title "")
  "**Parsing**"

  "**Part 1**"

  "**Part 2**")

(defun build-data (&optional input)
  (read-day-input #'identity :input input))

(defun part-1 (&optional (data (build-data)))
  (foo data))

(defun part-2 (&optional (data (build-data)))
  (bar data))
