(mgl-pax:define-package :aoc.2020.06
  (:nicknames :2020.06)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :serapeum :~>>))

(in-package :2020.06)

(defsection @2020.06 (:title "Custom Customs")
  (@part-1 section)
  (parse-group function)
  (@part-2 section)
  (count-groups function))

(defsection @part-1 (:title "Count any yes answers"))

(defun parse-group (group)
  (~>> group
       (cl-ppcre:split "\\n")
       (mapcar (lambda (e) (coerce e 'list)))))

(defun count-groups (groups combine-rows)
  (~>> groups
       (mapcar (lambda (x) (reduce combine-rows x)))
       (reduce #'+ _ :key #'length)))

(defsection @part-2 (:title "Count all yes answers"))

(defun part-1 ()
  (let ((groups (read-day-input #'parse-group :separator "\\n\\n")))
    (summarize (count-groups groups #'union))))

(defun part-2 ()
  (let ((groups (read-day-input #'parse-group :separator "\\n\\n")))
    (summarize (count-groups groups #'intersection))))
