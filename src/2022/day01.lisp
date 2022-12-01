(mgl-pax:define-package :aoc.2022.01
  (:nicknames :2022.01)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :cl-ppcre :split)
  (:import-from :serapeum :~>>
                          :take))

(in-package :2022.01)

(defsection @2022.01 (:title "Calorie Counting")
  "Requirements: [Day 01](https://adventofcode.com/2022/day/1)"
  (@part-1 section)
  (@part-2 section))

(defsection @part-1 (:title "Who got snacks?"))

(defun parse-inventory (inventory)
  (~>> (split "\\n" inventory)
       (mapcar #'parse-integer)
       (reduce #'+)))

(defun part-1 ()
  (let ((snacks (read-day-input #'parse-inventory :separator "\\n\\n")))
    (apply 'max snacks)))

(defsection @part-2 (:title "Backup Snack Strategy"))

(defun total-snacks (snacks)
  (~>> (sort snacks #'>)
       (take 3)
       (reduce #'+)))

(defun part-2 ()
  (let ((snacks (read-day-input #'parse-inventory :separator "\\n\\n")))
    (total-snacks snacks)))
