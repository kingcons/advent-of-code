(mgl-pax:define-package :aoc.2022.03
  (:nicknames :2022.03)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :serapeum #:~>>
                          #:halves
                          #:op))

(in-package :2022.03)

(defsummary (:title "Rucksack Reorganization")
  "**Part 1** - Misplaced Items"
  (locate-duplicate function)
  (locate-duplicate-source
   (include (:start (locate-duplicate function) :end (*priorities* variable))
            :header-nl "```common-lisp" :footer-nl "```"))

  "**Part 2** - Unauthenticated Badges"
  (locate-badge function)
  (locate-badge-source
   (include (:start (locate-badge function) :end (part-2 function))
            :header-nl "```common-lisp" :footer-nl "```")))

(defun locate-duplicate (rucksack)
  (let ((chars (coerce rucksack 'list)))
    (multiple-value-bind (compartment-a compartment-b) (halves chars)
      (first (intersection compartment-a compartment-b)))))

(defvar *priorities*
  (let ((table (make-hash-table :test #'eq)))
    (loop for code from 65 upto 90
          do (setf (gethash (code-char code) table) (- code 38)))
    (loop for code from 97 upto 122
          do (setf (gethash (code-char code) table) (- code 96)))
    table))

(defun total-priority (items)
  (reduce #'+ items :key (op (gethash _ *priorities*))))

(defun part-1 ()
  (let ((items (read-day-input #'locate-duplicate)))
    (total-priority items)))

(defun locate-badge (group)
  (first (reduce #'intersection group :key (op (coerce _ 'list)))))

(defun part-2 ()
  (let ((items (read-day-input #'locate-badge :batches-of 3)))
    (total-priority items)))
