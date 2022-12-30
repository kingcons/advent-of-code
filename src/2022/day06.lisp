(mgl-pax:define-package :aoc.2022.06
  (:nicknames :2022.06)
  (:import-from :alexandria #:setp)
  (:use :cl :aoc.util :mgl-pax))

(in-package :2022.06)

(defsummary (:title "Tuning Trouble")
  "**Parsing**"

  "**Part 1**"
  (part-1-source
   (include (:start (start-marker? function) :end (part-2 function))
            :header-nl "```common-lisp" :footer-nl "```"))

  "**Part 2**")

(defun build-data (&optional input)
  (first (read-day-input #'identity :input input)))

(defun start-marker? (input start width)
  (let ((buffer (coerce (subseq input (- start width) start) 'list)))
    (setp buffer)))

(defun parse-signal (input width)
  (loop for start = width then (1+ start)
        until (start-marker? input start width)
        finally (return start)))

(defun part-1 (&optional (data (build-data)))
  (parse-signal data 4))

(defun part-2 (&optional (data (build-data)))
  (parse-signal data 14))
