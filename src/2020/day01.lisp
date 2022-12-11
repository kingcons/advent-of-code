(mgl-pax:define-package :aoc.2020.01
  (:nicknames :2020.01)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :alexandria #:when-let))

(in-package :2020.01)

(defsummary (:title "Report Repair")
  "**Part 1** - Fix the Expense Report"
  (find-pair function)
  "**Part 2** - Now in triplicate"
  (find-triple function))

(defun build-data (&optional input)
  (read-day-input #'parse-integer :input input))

(defun find-pair (items)
  (dolist (item-1 items)
    (dolist (item-2 items)
      (when (= (+ item-1 item-2) 2020)
        (return-from find-pair (* item-1 item-2))))))

(defun part-1 (&optional (data (build-data)))
  (find-pair data))

(defun find-triple (items &aux (hash (make-hash-table)))
  (dolist (item items)
    (setf (gethash item hash) item))
  (dolist (item-1 items)
    (dolist (item-2 items)
      (when-let (item-3 (gethash (- 2020 item-1 item-2) hash))
        (return-from find-triple (* item-1 item-2 item-3))))))

(defun part-2 (&optional (data (build-data)))
  (find-triple data))
