(mgl-pax:define-package :aoc.2021.07
  (:nicknames :2021.07)
  (:use :cl :aoc.util :mgl-pax))

(in-package :2021.07)

(defsection @2021.07 (:title "The Treachery of Whales")
  (@part-1 section)
  (@part-2 section))

(defun parse-csv (input)
  (sort (coerce (mapcar #'parse-integer (cl-ppcre:split "," input)) 'vector) #'<))

(defun find-bounds (positions)
  (list (aref positions 0) (aref positions (1- (length positions)))))

(defun gauss-sum (n)
  (/ (* n (1+ n)) 2))

(defun build-costs (positions cost-function)
  (let ((bounds (find-bounds positions)))
    (loop with costs = (make-hash-table)
          for i from (first bounds) upto (second bounds)
          do (setf (gethash i costs)
                   (loop for position across positions
                         sum (funcall cost-function position i)))
          finally (return costs))))

(defun align-crabs (positions cost-function)
  (let ((costs (build-costs positions cost-function)))
    (loop with best = most-positive-fixnum
          with result = -1
          for key being the hash-keys in costs
          do (let ((cost (gethash key costs)))
               (when (< cost best)
                 (format t "~D:  ~D cheaper than ~D~%" key cost best)
                 (setf best cost
                       result key)))
          finally (return best))))

(defun part-1 ()
  (let ((data (first (read-day-input #'parse-csv))))
    (align-crabs data (lambda (position i) (abs (- position i))))))

(defun part-2 ()
  (let ((data (first (read-day-input #'parse-csv))))
    (align-crabs data (lambda (position i) (gauss-sum (abs (- position i)))))))
