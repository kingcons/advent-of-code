(mgl-pax:define-package :aoc.2023.04
  (:nicknames :2023.04)
  (:use :cl :mgl-pax :aoc.util :esrap)
  (:import-from :serapeum :op))

(in-package :2023.04)

(defsummary (:title "Scratchcards")
  "**Parsing**"

  "**Part 1**"

  "**Part 2**")

(defrule card-id (and "Card" (+ #\Space) integer ": ")
  (:function third))

(defrule numbers (+ (or integer #\Space))
  (:lambda (list) (remove " " list :test #'equal)))

(defrule scratchcard (and card-id numbers "|" numbers)
  (:destructure (id winning separator numbers)
    (declare (ignore separator))
    (list id winning numbers)))

(defun build-data (&optional input)
  (read-day-input (op (parse 'scratchcard _)) :input input))

(defun score-row (row)
  (destructuring-bind (id wins nums) row
    (declare (ignore id))
    (let ((matches (length (intersection wins nums))))
      (round (expt 2 (1- matches))))))

(defun part-1 (&optional (data (build-data)))
  (reduce #'+ data :key #'score-row))

(defun count-cards (data)
  (loop with counts = (make-array (length data) :initial-element 1)
        for (row wins nums) in data
        for matches = (length (intersection wins nums))
        do (dotimes (i matches)
             (incf (aref counts (+ i row 1)) (aref counts row)))
        finally (return counts)))

(defun part-2 (&optional (data (build-data)))
  (reduce #'+ (count-cards data)))
