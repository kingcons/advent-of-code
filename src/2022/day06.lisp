(mgl-pax:define-package :aoc.2022.06
  (:nicknames :2022.06)
  (:import-from :alexandria #:setp)
  (:use :cl :aoc.util :mgl-pax))

(in-package :2022.06)

(defsummary (:title "Tuning Trouble")
  "**Part 1** - "

  "**Part 2** - ")

(defun start-marker? (input start width)
  (let ((buffer (coerce (subseq input (- start width) start) 'list)))
    (setp buffer)))

(defun parse-signal (input width)
  (loop for start = width then (1+ start)
        until (start-marker? input start width)
        finally (return start)))

(defun part-1 ()
  (let ((data (first (read-day-input #'identity))))
    (parse-signal data 4)))

(defun part-2 ()
  (let ((data (first (read-day-input #'identity))))
    (parse-signal data 14)))
