(mgl-pax:define-package :aoc.2020.02
  (:nicknames :2020.02)
  (:use :cl :aoc.util :mgl-pax))

(in-package :2020.02)

(defsummary (:title "Password Philosophy")
  "**Part 1** - Count valid passwords"
  (count-valid function)
  "**Part 2** - Count using XOR"
  (count-xor-valid function))

(defun parse-password (row)
  (cl-ppcre:register-groups-bind ((#'parse-integer min max) letter password)
      ("(\\d+)-(\\d+) (\\w): (\\w+)" row)
    (list min max letter password)))

(defun count-valid (items)
  (loop for (min max letter password) in items
        counting (let ((count (count letter password :test #'string=)))
                   (<= min count max))))

(defun part-1 ()
  (let ((items (read-day-input #'parse-password)))
    (count-valid items)))

(defun xor-match? (min max letter password)
  (alexandria:xor (string= letter password :start2 (1- min) :end2 min)
                  (string= letter password :start2 (1- max) :end2 max)))

(defun count-xor-valid (items)
  (loop for (min max letter password) in items
        counting (xor-match? min max letter password)))

(defun part-2 ()
  (let ((items (read-day-input #'parse-password)))
    (count-xor-valid items)))
