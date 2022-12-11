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

(defun build-data (&optional input)
  (read-day-input #'parse-group :separator "\\n\\n" :input input))

(defun part-1 (&optional (data (build-data)))
  (count-groups data #'union))

(defun part-2 (&optional (data (build-data)))
  (count-groups data #'intersection))
