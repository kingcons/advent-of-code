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
  "Day 3 supplies us with random-seeming strings and asks us to find the
duplicated character in the first and second half of the string. Then we should
score and sum those characters. This is easy when modeled as a set intersection
of two lists of characters."

  "**Parsing**

Parsing is trivial. Just split the input by newlines. This is the default
behavior of READ-DAY-INPUT so we pass IDENTITY as the callback."

  (parsing-source
   (include (:start (build-data function) :end (*priorities* variable))
            :header-nl "```common-lisp" :footer-nl "```"))

  "**Part 1**

The most annoying aspect of part 1 was the arbitrary scoring they
came up with that was related to but did not quite match ASCII values.
The *PRIORITIES* table deals with this and makes score lookup easy.

The SOLVE function handles the rest of the problem. It runs a preprocess-fn on
the parsed input, then maps over it with SEARCH-RUCKSACK to find the duplicated
character. Finally, we run the characters through TOTAL-PRIORITY for scoring.
In part 1, our preprocess-fn simply splits the input lines into two halves."
  (part-1-source
   (include (:start (*priorities* variable) :end (part-2 function))
            :header-nl "```common-lisp" :footer-nl "```"))

  "**Part 2**

Part 2 doesn't require any special handling at all.
We simply call solve as we did in part-1 but with a different `preprocess-fn`,
asking it to group things with `(op (serapeum:batches _ 3))`.")

(defun build-data (&optional input)
  (read-day-input #'identity :input input))

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

(defun solve (data preprocess-fn)
  (~>> (funcall preprocess-fn data)
       (mapcar #'search-rucksack)
       (total-priority)))

(defun part-1 (&optional (data (build-data)))
  (solve data (op (mapcar (lambda (x) (multiple-value-list (halves x))) _))))

(defun part-2 (&optional (data (build-data)))
  (solve data (op (serapeum:batches _ 3))))
