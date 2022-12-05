(mgl-pax:define-package :aoc.2022.05
  (:nicknames :2022.05)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :alexandria #:assoc-value)
  (:import-from :cl-ppcre #:register-groups-bind
                #:split)
  (:import-from :serapeum #:~>>
                          #:lines
                          #:op))

(in-package :2022.05)

(defsummary (:title "Supply Stacks")
  "**Part 1** - "

  "**Part 2** - ")

(defparameter *stacks*
  '((1 . (p z m t r c n))
    (2 . (z b s t n d))
    (3 . (g t c f r q h m))
    (4 . (z r g))
    (5 . (h r n z))
    (6 . (d l z p w s h f))
    (7 . (m g c r z d w))
    (8 . (q z w h l f j s))
    (9 . (n w p q s))))

(defun parse-stacks (input)
  (destructuring-bind (header moves) input
    (declare (ignore header))
    (let ((steps (lines moves)))
      (loop for step in steps
            collect (register-groups-bind ((#'parse-integer count origin destination))
                        ("move (\\d+) from (\\d+) to (\\d+)" step)
                      (list count origin destination))))))

(defun move-crates (count origin destination)
  (dotimes (i count)
    (let ((item (pop (assoc-value *stacks* origin))))
      (push item (assoc-value *stacks* destination)))))

(defun interpret (moves stacks &key step-fn)
  (loop for (count origin destination) in moves
        do (funcall step-fn count origin destination))
  (loop for i from 1 upto 9
        collecting (first (assoc-value stacks i))))

(defun part-1 ()
  (let ((data (read-day-input #'parse-stacks :separator "\\n\\n" :whole t)))
    (interpret data *stacks* :step-fn #'move-crates)))

(defun move-crates-contiguous (count origin destination)
  (let ((to-move (loop for i below count
                       collecting (pop (assoc-value *stacks* origin)))))
    (loop for crate in (reverse to-move)
          do (push crate (assoc-value *stacks* destination)))))

(defun part-2 ()
  (let ((data (read-day-input #'parse-stacks :separator "\\n\\n" :whole t)))
    (interpret data *stacks* :step-fn #'move-crates-contiguous)))
