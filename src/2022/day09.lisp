(mgl-pax:define-package :aoc.2022.09
  (:nicknames :2022.09)
  (:use :cl :mgl-pax :aoc.util :esrap)
  (:import-from :aoc.parsers #:integer)
  (:import-from :alexandria #:emptyp)
  (:import-from :serapeum #:partial))

(in-package :2022.09)

(defsummary (:title "Rope Bridge")
  "**Part 1** - "

  "**Part 2** - ")

(defstruct (point (:type list)) x y)

(defrule direction (or #\U #\R #\D #\L))

(defrule motion (and direction " " integer)
  (:lambda (list) (remove " " list :test #'equal)))

(defun build-data (&optional input)
  (read-day-input (partial #'parse 'motion) :input input))

(defun move-head (point direction)
  (case (char direction 0)
    (#\U (incf (point-y point)))
    (#\R (incf (point-x point)))
    (#\D (decf (point-y point)))
    (#\L (decf (point-x point))))
  point)

(defun move-tail (head tail)
  (let* ((new-tail (copy-list tail))
         (x-diff (- (point-x head) (point-x tail)))
         (y-diff (- (point-y head) (point-y tail)))
         (snap-x? (and (> (abs y-diff) 1)
                       (not (zerop x-diff))))
         (snap-y? (and (> (abs x-diff) 1)
                       (not (zerop y-diff)))))
    (cond ((> x-diff  1) (incf (point-x new-tail)))
          ((< x-diff -1) (decf (point-x new-tail)))
          ((> y-diff  1) (incf (point-y new-tail)))
          ((< y-diff -1) (decf (point-y new-tail))))
    (cond (snap-x? (setf (point-x new-tail) (point-x head)))
          (snap-y? (setf (point-y new-tail) (point-y head))))
    new-tail))

(defun count-visited (moves)
  (let ((visited (make-hash-table :test #'equal))
        (head (make-point :x 0 :y 0))
        (tail (make-point :x 0 :y 0)))
    (loop for (direction amount) in moves
          do (loop repeat amount
                   do (setf head (move-head head direction)
                            tail (move-tail head tail)
                            (gethash tail visited) t)))
    (hash-table-count visited)))

(defun part-1 (&optional input)
  (count-visited (build-data input)))

(defun part-2 (&optional input)
  (count-visited (build-data input)))
