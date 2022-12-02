(mgl-pax:define-package :aoc.2020.06
  (:nicknames :2020.06)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :serapeum :~>>))

(in-package :2020.06)

(defsection @2020.06 (:title "Custom Customs")
  "Requirements: [Day 06](https://adventofcode.com/2020/day/6)"

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
    (summarize (count-groups groups #'union))))

(defun part-2 ()
  (let ((groups (read-day-input #'parse-group :separator "\\n\\n")))
    (summarize (count-groups groups #'intersection))))
