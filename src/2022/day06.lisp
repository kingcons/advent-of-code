(mgl-pax:define-package :aoc.2022.06
  (:nicknames :2022.06)
  (:import-from :alexandria #:setp)
  (:use :cl :aoc.util :mgl-pax))

(in-package :2022.06)

(defsummary (:title "Tuning Trouble")
  "Day 6 is straightforward both in terms of parsing and logic.
The input data is a single long string and we are asked to find a run
of distinct characters of a certain size."

  "**Parsing**

We lean on the default READ-DAY-INPUT and IDENTITY behavior again."

  "**Part 1**

Part 1 is pretty straightforward thanks to the combination of SETP and LOOP.
We loop until we find a distinct run of characters, checked for by
PACKET-MARKER?, always looking back to avoid running off the end of the input."
  (part-1-source
   (include (:start (packet-marker? function) :end (part-2 function))
            :header-nl "```common-lisp" :footer-nl "```"))

  "**Part 2**

Part 2 only asks for a different marker width which was easy to parameterize
out of the original code. Pass 14 instead of 4 and we're all done.")

(defun build-data (&optional input)
  (first (read-day-input #'identity :input input)))

(defun packet-marker? (input end width)
  (let ((buffer (coerce (subseq input (- end width) end) 'list)))
    (setp buffer)))

(defun find-signal-marker (input width)
  (loop for end = width then (1+ end)
        until (packet-marker? input end width)
        finally (return end)))

(defun part-1 (&optional (data (build-data)))
  (find-signal-marker data 4))

(defun part-2 (&optional (data (build-data)))
  (find-signal-marker data 14))
