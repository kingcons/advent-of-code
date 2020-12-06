(defpackage :advent2020.day-06
  (:nicknames :day-06)
  (:use :cl)
  (:import-from :advent2020.util #:read-day-input)
  (:export))

(defun parse-group-any (group)
  (loop with hash = (make-hash-table)
        for char across group
        when (alpha-char-p char)
          do (setf (gethash char hash) char)
        finally (return hash)))

(defun parse-group-all (group)
  (let* ((entries (cl-ppcre:split "\\n" group))
         (sets (mapcar (lambda (e) (coerce e 'list)) entries)))
    (reduce #'intersection sets)))

(defun part-1 ()
  (let ((groups (read-day-input 6 #'parse-group-any :separator "\\n\\n")))
    (reduce #'+ (mapcar #'hash-table-count groups))))

(defun part-2 ()
  (let ((groups (read-day-input 6 #'parse-group-all :separator "\\n\\n")))
    (reduce #'+ (mapcar #'length groups))))
