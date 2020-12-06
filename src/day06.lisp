(defpackage :advent2020.day-06
  (:nicknames :day-06)
  (:use :cl)
  (:import-from :advent2020.util #:read-day-input)
  (:import-from :arrows #:->> #:-<>>)
  (:export))

(defun parse-group (group)
  (->> group
       (cl-ppcre:split "\\n")
       (mapcar (lambda (e) (coerce e 'list)))))

(defun count-groups (groups combine-rows)
  (-<>> groups
        (mapcar (lambda (x) (reduce combine-rows x)))
        (reduce #'+ <> :key #'length)))

(defun part-1 ()
  (let ((groups (read-day-input 6 #'parse-group :separator "\\n\\n")))
    (count-groups groups #'union)))

(defun part-2 ()
  (let ((groups (read-day-input 6 #'parse-group :separator "\\n\\n")))
    (count-groups groups #'intersection)))
