(defpackage :aoc.2020.06
  (:nicknames :2020.06)
  (:use :cl :aoc.util)
  (:import-from :arrows #:->> #:-<>>)
  (:export #:parse-group #:count-groups))

(in-package :2020.06)

(defun parse-group (group)
  (->> group
       (cl-ppcre:split "\\n")
       (mapcar (lambda (e) (coerce e 'list)))))

(defun count-groups (groups combine-rows)
  (-<>> groups
        (mapcar (lambda (x) (reduce combine-rows x)))
        (reduce #'+ <> :key #'length)))

(defun part-1 ()
  (let ((groups (read-day-input #'parse-group :separator "\\n\\n")))
    (count-groups groups #'union)))

(defun part-2 ()
  (let ((groups (read-day-input #'parse-group :separator "\\n\\n")))
    (count-groups groups #'intersection)))
