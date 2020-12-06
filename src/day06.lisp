(defpackage :advent2020.day-06
  (:nicknames :day-06)
  (:use :cl)
  (:import-from :advent2020.util #:read-day-input)
  (:import-from :arrows #:->> #:-<>>)
  (:export))

(defun parse-group-any (group)
  (-<>> group
       (remove-duplicates)
       (remove-if-not #'alpha-char-p)
       (coerce <> 'list)))

(defun parse-group-all (group)
  (->> group
       (cl-ppcre:split "\\n")
       (mapcar (lambda (e) (coerce e 'list)))
       (reduce #'intersection)))

(defun part-1 ()
  (let ((groups (read-day-input 6 #'parse-group-any :separator "\\n\\n")))
    (reduce #'+ (mapcar #'length groups))))

(defun part-2 ()
  (let ((groups (read-day-input 6 #'parse-group-all :separator "\\n\\n")))
    (reduce #'+ (mapcar #'length groups))))
