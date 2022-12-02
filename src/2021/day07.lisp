(mgl-pax:define-package :aoc.2021.07
  (:nicknames :2021.07)
  (:use :cl :aoc.util :mgl-pax))

(in-package :2021.07)

(defsection @2021.07 (:title "The Treachery of Whales")
  "Requirements: [Day 07](https://adventofcode.com/2021/day/7)"

  "**Part 1** - Do the Crab Claw"
  (align-crabs function)
  "**Part 2** - Crabs Engineer Different"
  (min-distance-gauss function))

(defun parse-csv (input)
  (sort (coerce (mapcar #'parse-integer (cl-ppcre:split "," input)) 'vector) #'<))

(defun align-crabs (positions cost-function target)
  (loop for position across positions
        sum (funcall cost-function position target)))

(defun part-1 ()
  (let* ((data (first (read-day-input #'parse-csv)))
         (median (aref data (floor (length data) 2))))
    (summarize (align-crabs data #'min-distance median))))

(defun gauss-sum (n)
  (/ (* n (1+ n)) 2))

(defun min-distance (position i)
  (abs (- position i)))

(defun min-distance-gauss (position i)
  (gauss-sum (min-distance position i)))

(defun part-2 ()
  (let* ((data (first (read-day-input #'parse-csv)))
         (mean (floor (reduce #'+ data) (length data))))
    (summarize (align-crabs data #'min-distance-gauss mean))))
