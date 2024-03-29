(mgl-pax:define-package :aoc.2021.06
  (:nicknames :2021.06)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :alexandria #:rotate)
  (:import-from :cl-ppcre #:split))

(in-package :aoc.2021.06)

(defsummary (:title "Lanternfish")
  "**Part 1** - That's a big school"
  (tick function)
  "**Part 2** - Uh oh"
  (estimate-population function))

(defun parse-counts (fishlist)
  (let ((counts (make-array 9 :element-type 'fixnum))
        (timers (mapcar #'parse-integer (split "," fishlist))))
    (dolist (timer timers)
      (incf (aref counts timer)))
    counts))

(defun build-data (&optional input)
  (first (read-day-input #'parse-counts :input input)))

(defun tick (counts)
  (declare (type (simple-array fixnum) counts))
  (rotate counts -1)
  (let ((newborns (aref counts 8))
        (old-gen (aref counts 6)))
    (setf (aref counts 6) (+ old-gen newborns))))

(defun estimate-population (counts days)
  (dotimes (i days)
    (tick counts))
  (reduce #'+ counts))

(defun part-1 (&optional (data (build-data)))
  (estimate-population data 80))

(defun part-2 (&optional (data (build-data)))
  (estimate-population data 256))
