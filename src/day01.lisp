(defpackage :advent2020.day-01
  (:nicknames :day-01)
  (:use :cl)
  (:import-from :alexandria #:read-file-into-string)
  (:import-from :split-sequence #:split-sequence)
  (:export #:fix-expense-report))

(in-package :day-01)

(defun fix-expense-report (items)
  (dolist (item-1 items)
    (dolist (item-2 items)
      (when (= (+ item-1 item-2) 2020)
        (return-from fix-expense-report (* item-1 item-2))))))

(defun part01 (file)
  (let* ((pathname (asdf:system-relative-pathname :advent2020 file))
         (input (split-sequence #\Newline (read-file-into-string pathname)))
         (items (mapcar #'parse-integer (butlast input))))
    (fix-expense-report items)))
