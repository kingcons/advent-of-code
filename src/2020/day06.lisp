(mgl-pax:define-package :aoc.2020.06
  (:nicknames :2020.06)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :serapeum :~>>))

(in-package :2020.06)

(defsummary (:title "Custom Customs")
  "**Part 1** - Count any yes answers"
  (parse-group function)
  "**Part 2** - Count all yes answers"
  (count-groups function))

(defun parse-group (group)
  (~>> group
       (cl-ppcre:split "\\n")
       (mapcar (lambda (e) (coerce e 'list)))))

(defun count-groups (groups combine-rows)
  (~>> groups
       (mapcar (lambda (x) (reduce combine-rows x)))
       (reduce #'+ _ :key #'length)))

(defun part-1 ()
  (let ((groups (read-day-input #'parse-group :separator "\\n\\n")))
    (count-groups groups #'union)))

(defun part-2 ()
  (let ((groups (read-day-input #'parse-group :separator "\\n\\n")))
    (count-groups groups #'intersection)))
