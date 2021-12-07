(mgl-pax:define-package :aoc.2021.07
  (:nicknames :2021.07)
  (:use :cl :aoc.util :mgl-pax))

(in-package :2021.07)

(defsection @2021.07 (:title "The Treachery of Whales")
  (@part-1 section)
  (@part-2 section))

(defsection @prt-1 (:title "Do the Crab Claw"))

(defun parse-csv (input)
  (sort (coerce (mapcar #'parse-integer (cl-ppcre:split "," input)) 'vector) #'<))

(defun build-costs (positions cost-function)
  (let ((length (length positions)))
    (loop with costs = (make-hash-table)
          for i from (aref positions 0) upto (aref positions (1- length))
          do (setf (gethash i costs)
                   (loop for position across positions
                         sum (funcall cost-function position i)))
          finally (return costs))))

(defun align-crabs (positions cost-function)
  (let ((costs (build-costs positions cost-function)))
    (loop for key being the hash-keys in costs
          minimizing (gethash key costs))))

(defun part-1 ()
  (let ((data (first (read-day-input #'parse-csv))))
    (summarize (align-crabs data #'min-distance))))

(defsection @part-2 (:title "Crabs Engineer Different"))

(defun gauss-sum (n)
  (/ (* n (1+ n)) 2))

(defun min-distance (position i)
  (abs (- position i)))

(defun min-distance-gauss (position i)
  (gauss-sum (min-distance position i)))

(defun part-2 ()
  (let ((data (first (read-day-input #'parse-csv))))
    (summarize (align-crabs data #'min-distance-gauss))))
