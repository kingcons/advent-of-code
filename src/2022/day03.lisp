(mgl-pax:define-package :aoc.2022.03
  (:nicknames :2022.03)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :serapeum
                #:~>>
                #:batches
                #:halves
                #:op))

(in-package :2022.03)

(defsummary (:title "Rucksack Reorganization")
  "**Part 1** - Misplaced Items"
  (search-rucksack function)
  (part-1-source
   (include (:start (search-rucksack function) :end (build-data function))
            :header-nl "```common-lisp" :footer-nl "```"))

  "**Part 2** - Unauthenticated Badges"
  (solve function)
  (part-2-source
   (include (:start (solve function) :end (part-1 function))
            :header-nl "```common-lisp" :footer-nl "```")))

(defvar *priorities*
  (let ((table (make-hash-table :test #'eq)))
    (loop for code from 65 upto 90
          do (setf (gethash (code-char code) table) (- code 38)))
    (loop for code from 97 upto 122
          do (setf (gethash (code-char code) table) (- code 96)))
    table))

(defun search-rucksack (group)
  (first (reduce #'intersection group :key (op (coerce _ 'list)))))

(defun total-priority (items)
  (reduce #'+ items :key (op (gethash _ *priorities*))))

(defun build-data (&optional input)
  (read-day-input #'identity :input input))

(defun solve (input preprocess-fn)
  (~>> (build-data input)
       (funcall preprocess-fn)
       (mapcar #'search-rucksack)
       (total-priority)))

(defun part-1 (&optional input)
  (solve input (op (mapcar (lambda (x) (multiple-value-list (halves x))) _))))

(defun part-2 (&optional input)
  (solve input (op (serapeum:batches _ 3))))
