(mgl-pax:define-package :aoc.2021.07
  (:nicknames :2021.07)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :cl-ppcre #:split))

(in-package :2021.07)

(defsummary (:title "The Treachery of Whales")
  "**Part 1** - Do the Crab Claw"
  (align-crabs function)
  "**Part 2** - Crabs Engineer Different"
  (min-distance-gauss function))

(defun parse-csv (input)
  (sort (coerce (mapcar #'parse-integer (split "," input)) 'vector) #'<))

(defun build-data (&optional input)
  (first (read-day-input #'parse-csv :input input)))

(defun align-crabs (positions cost-function target)
  (loop for position across positions
        sum (funcall cost-function position target)))

(defun part-1 (&optional (data (build-data)))
  (let ((median (aref data (floor (length data) 2))))
    (align-crabs data #'min-distance median)))

(defun gauss-sum (n)
  (/ (* n (1+ n)) 2))

(defun min-distance (position i)
  (abs (- position i)))

(defun min-distance-gauss (position i)
  (gauss-sum (min-distance position i)))

(defun part-2 (&optional (data (build-data)))
  (let ((mean (floor (reduce #'+ data) (length data))))
    (align-crabs data #'min-distance-gauss mean)))
