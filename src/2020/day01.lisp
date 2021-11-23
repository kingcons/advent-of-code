(defpackage :aoc.2020.01
  (:nicknames :2020.01)
  (:use :cl :aoc.util)
  (:import-from :alexandria #:when-let)
  (:export #:find-pair #:find-triple))

(in-package :2020.01)

(defun find-pair (items)
  (dolist (item-1 items)
    (dolist (item-2 items)
      (when (= (+ item-1 item-2) 2020)
        (return-from find-pair (* item-1 item-2))))))

(defun find-triple (items &aux (hash (make-hash-table)))
  (dolist (item items)
    (setf (gethash item hash) item))
  (dolist (item-1 items)
    (dolist (item-2 items)
      (when-let (item-3 (gethash (- 2020 item-1 item-2) hash))
        (return-from find-triple (* item-1 item-2 item-3))))))

(defun part-1 ()
  (let ((items (read-day-input #'parse-integer)))
    (find-pair items)))

(defun part-2 ()
  (let ((items (read-day-input #'parse-integer)))
    (find-triple items)))
