(mgl-pax:define-package :aoc.2020.01
  (:nicknames :2020.01)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :alexandria #:when-let))

(in-package :2020.01)

(defsection @2020.01 (:title "Report Repair")
  (@part-1 section)
  (find-pair function)
  (@part-2 section)
  (find-triple function))

(defsection @part-1 (:title "Fix the Expense Report"))

(defun find-pair (items)
  (dolist (item-1 items)
    (dolist (item-2 items)
      (when (= (+ item-1 item-2) 2020)
        (return-from find-pair (* item-1 item-2))))))

(defun part-1 ()
  (let ((items (read-day-input #'parse-integer)))
    (summarize (find-pair items))))

(defsection @part-2 (:title "Now in triplicate"))

(defun find-triple (items &aux (hash (make-hash-table)))
  (dolist (item items)
    (setf (gethash item hash) item))
  (dolist (item-1 items)
    (dolist (item-2 items)
      (when-let (item-3 (gethash (- 2020 item-1 item-2) hash))
        (return-from find-triple (* item-1 item-2 item-3))))))

(defun part-2 ()
  (let ((items (read-day-input #'parse-integer)))
    (summarize (find-triple items))))
