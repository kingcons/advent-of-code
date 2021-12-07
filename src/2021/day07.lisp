(mgl-pax:define-package :aoc.2021.07
  (:nicknames :2021.07)
  (:use :cl :aoc.util :mgl-pax))

(in-package :2021.07)

(defsection @2021.07 (:title "The Treachery of Whales")
  (@part-1 section)
  (align-crabs function)
  (@part-2 section)
  (min-distance-gauss function))

(defsection @part-1 (:title "Do the Crab Claw"))

(defun parse-csv (input)
  (sort (coerce (mapcar #'parse-integer (cl-ppcre:split "," input)) 'vector) #'<))

(defun align-crabs (positions cost-function target)
  (loop for position across positions
        sum (funcall cost-function position target)))

(defun part-1 ()
  (let* ((data (first (read-day-input #'parse-csv)))
         (median (aref data (floor (length data) 2))))
    (summarize (align-crabs data #'min-distance median))))

(defsection @part-2 (:title "Crabs Engineer Different"))

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
