(mgl-pax:define-package :aoc.2022.05
  (:nicknames :2022.05)
  (:use :cl :mgl-pax :aoc.util :aoc.parsers :esrap)
  (:import-from :serapeum
                #:~>>
                #:batches
                #:dict
                #:fmt
                #:halves
                #:op))

(in-package :2022.05)

(defsummary (:title "Supply Stacks")
  "Day 5 asks us what message is shown after performing a series of crane moves
on stacks of labeled shipping containers. This is pretty simple to model with
a map of integers to stacks or an array of stacks. Unfortunately, parsing the
representation for the day is not so simple."

  "**Parsing**

We break out [esrap](https://scymtym.github.io/esrap/) again to build a
robust parser for this format. We separate it into rules for the header
consisting of several rows of crates, the labels for those rows, and the
instructions for how to move the crane. PARSE-STACKS takes the extracted
values from esrap's parse step and constructs a hash table with stacks
matching the ASCII depiction in the header."
  (parsing-source
   (include (:start (*first-rule* variable) :end (move-crates function))
            :header-nl "```common-lisp" :footer-nl "```"))

  "**Part 1**

The actual work of this problem is simple. We have a list of moves to perform.
We want to move a certain number of crates from stack A to stack B which is
handled by MOVE-CRATES. Running all the instructions and reading the labels
of the top crates on each stack is performed by INTERPRET."
  (part-1-source
   (include (:start (move-crates function) :end (move-crates-contiguous function))
            :header-nl "```common-lisp" :footer-nl "```"))

  "**Part 2**

Part 2 complicates matters very slightly by asking that we move the crates all
at once rather than one at a time. This is handled by MOVE-CRATES-CONTIGUOUS."
  (part-2-source
   (include (:start (move-crates-contiguous function) :end (part-2 function))
            :header-nl "```common-lisp" :footer-nl "```")))

(defvar *first-rule*
  (defrule move (and "move " integer " from " integer " to " integer (? #\Newline))
    (:lambda (list)
      (remove-if-not #'integerp list))))

(defrule instructions (+ move))

(defrule crate (and "[" letter "]" whitespace)
  (:function second))

(defrule gap (and whitespace whitespace whitespace whitespace)
  (:constant :gap))

(defrule row (+ (or crate gap)))

(defrule labels (+ (or whitespace integer #\Newline))
  (:lambda (list)
    (remove-if-not #'integerp list)))

(defrule header (+ row)
  (:function first))

(defrule stacks (and header labels instructions))

(defun add-row (row table)
  (loop for i = 1 then (1+ i)
        for item in row
        when (characterp item)
          do (push item (gethash i table))))

(defun parse-stacks (input)
  (destructuring-bind (headers labels moves) (parse 'stacks input)
    (let* ((stack-count (length labels))
           (stacks (make-hash-table)))
      (loop for row in (reverse (batches headers stack-count))
            do (add-row row stacks))
      (list moves stacks))))

(defun build-data (&optional input)
  (read-day-input #'parse-stacks :whole t :input input))

(defun move-crates (count origin destination stacks)
  (dotimes (i count)
    (let ((item (pop (gethash origin stacks))))
      (push item (gethash destination stacks)))))

(defun interpret (data step-fn)
  (destructuring-bind (moves stacks) data
    (loop for (count origin destination) in moves
          do (funcall step-fn count origin destination stacks))
    (loop for i from 1 upto (hash-table-count stacks)
          collecting (first (gethash i stacks)) into chars
          finally (return (coerce chars 'string)))))

(defun part-1 (&optional (data (build-data)))
  (interpret data #'move-crates))

(defun move-crates-contiguous (count origin destination stacks)
  (multiple-value-bind (to-move new-origin) (halves (gethash origin stacks) count)
    (setf (gethash origin stacks) new-origin
          (gethash destination stacks) (append to-move (gethash destination stacks)))))

(defun part-2 (&optional (data (build-data)))
  (interpret data #'move-crates-contiguous))
