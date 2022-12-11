(mgl-pax:define-package :aoc.2022.09
  (:nicknames :2022.09)
  (:use :cl :mgl-pax :aoc.util :esrap)
  (:import-from :aoc.parsers #:integer)
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
         (distance (floor (sqrt (+ (expt x-diff 2) (expt y-diff 2))))))
    (when (> distance 1)
      (cond ((and (> x-diff  1) (zerop y-diff)) (incf (point-x new-tail)))
            ((and (< x-diff -1) (zerop y-diff)) (decf (point-x new-tail)))
            ((and (> y-diff  1) (zerop x-diff)) (incf (point-y new-tail)))
            ((and (< y-diff -1) (zerop x-diff)) (decf (point-y new-tail)))
            ((and (plusp x-diff) (plusp y-diff))
             (incf (point-x new-tail))
             (incf (point-y new-tail)))
            ((and (minusp x-diff) (minusp y-diff))
             (decf (point-x new-tail))
             (decf (point-y new-tail)))
            ((and (plusp x-diff) (minusp y-diff))
             (incf (point-x new-tail))
             (decf (point-y new-tail)))
            ((and (minusp x-diff) (plusp y-diff))
             (decf (point-x new-tail))
             (incf (point-y new-tail)))))
    new-tail))

(defun update-rope (move rope visited)
  (destructuring-bind (direction distance) move
    (dotimes (i distance)
      (setf (first rope) (move-head (first rope) direction))
      (loop for (head tail) on rope while tail
            for i = 1 then (1+ i)
            do (setf (nth i rope) (move-tail head tail))
            finally (setf (gethash head visited) t)))
    rope))

(defun count-visited (moves &key (tail-count 1))
  (let ((visited (make-hash-table :test #'equal))
        (rope (loop repeat (1+ tail-count) collect (make-point :x 0 :y 0))))
    (dolist (move moves)
      (update-rope move rope visited))
    (hash-table-count visited)))

(defun part-1 (&optional (data (build-data)))
  (count-visited data))

(defun part-2 (&optional (data (build-data)))
  (count-visited data :tail-count 9))
