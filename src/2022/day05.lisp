(mgl-pax:define-package :aoc.2022.05
  (:nicknames :2022.05)
  (:use :cl :aoc.util :mgl-pax :esrap)
  (:import-from :serapeum
                #:~>>
                #:batches
                #:fmt
                #:dict
                #:op))

(in-package :2022.05)

(defsummary (:title "Supply Stacks")
  "**Part 1** - "

  "**Part 2** - ")

(defrule whitespace (or #\Space #\Tab #\Newline))

(defrule letter (or (character-ranges (#\A #\Z))
                    (character-ranges (#\a #\z))))

(defrule digit (+ (character-ranges (#\0 #\9)))
  (:text t)
  (:function parse-integer))

(defrule move (and "move " digit " from " digit " to " digit (? #\Newline))
  (:lambda (list)
    (remove-if-not #'integerp list)))

(defrule instructions (+ move))

(defrule crate (and "[" letter "]" whitespace)
  (:function second))

(defrule gap (and whitespace whitespace whitespace whitespace)
  (:constant :gap))

(defrule row (+ (or crate gap)))

(defrule labels (+ (or whitespace digit))
  (:lambda (list)
    (remove-if-not #'integerp list)))

(defrule header (+ (or row labels)))

(defun add-row (row table)
  (loop for i = 1 then (1+ i)
        for item in row
        when (characterp item)
          do (push item (gethash i table))))

(defun parse-stacks (input)
  (let ((moves (parse 'instructions (second input))))
    (destructuring-bind (rows labels) (parse 'header (first input))
      (let* ((stack-count (length labels))
             (stacks (make-hash-table)))
        (loop for row in (reverse (batches rows stack-count))
              do (add-row row stacks))
        (list moves stacks)))))

(defun move-crates (count origin destination stacks)
  (dotimes (i count)
    (let ((item (pop (gethash origin stacks))))
      (push item (gethash destination stacks)))))

(defun interpret (data &key step-fn)
  (destructuring-bind (moves stacks) data
    (loop for (count origin destination) in moves
          do (funcall step-fn count origin destination stacks))
    (loop for i from 1 upto (hash-table-count stacks)
          collecting (first (gethash i stacks)) into chars
          finally (return (coerce chars 'string)))))

(defun build-data (&optional input)
  (read-day-input #'parse-stacks :separator "\\n\\n" :whole t :input input))

(defun part-1 (&optional (data (build-data)))
  (interpret data :step-fn #'move-crates))

(defun move-crates-contiguous (count origin destination stacks)
  (let ((to-move (loop for i below count
                       collecting (pop (gethash origin stacks)))))
    (loop for crate in (reverse to-move)
          do (push crate (gethash destination stacks)))))

(defun part-2 (&optional (data (build-data)))
  (interpret data :step-fn #'move-crates-contiguous))
