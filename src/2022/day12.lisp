(mgl-pax:define-package :aoc.2022.12
  (:nicknames :2022.12)
  (:use :cl :mgl-pax :aoc.util)
  (:import-from :alexandria
                #:define-constant
                #:removef)
  (:import-from :cl-ppcre #:split)
  (:import-from :serapeum #:op))

(in-package :2022.12)

(defsummary (:title "")
  "**Part 1** - "

  "**Part 2** - ")

(define-constant +neighbors+
    '((0 1) (0 -1) (1 0) (-1 0)) :test #'equal)

(defstruct point x y value)

(defun build-point (col row value)
  (make-point :x col :y row :value value))

(defmethod height ((point point))
  (with-slots (value) point
    (case value
      (#\S (char-code #\a))
      (#\E (char-code #\z))
      (t (char-code value)))))

(defmethod valid-step? ((point point) (match point))
  (>= 1 (- (height match)
           (height point))))

(defmethod get-neighbors ((point point) nodes)
  (with-slots (x y value) point
    (loop for (x-diff y-diff) in +neighbors+
          for coords = (cons (+ x x-diff)
                             (+ y y-diff))
          for match = (gethash coords nodes)
          when (and match (valid-step? point match))
            collect match)))

(defun parse-graph (input)
  (let ((nodes (make-hash-table :test #'equal))
        (edges (make-hash-table :test #'equal)))
    (flet ((import-row (row-idx row)
             (loop for col-idx = 0 then (1+ col-idx)
                   for char across row
                   do (let ((point (build-point col-idx row-idx char)))
                        (setf (gethash (cons col-idx row-idx) nodes) point)
                        (case char
                          (#\S (setf (gethash :start nodes) point))
                          (#\E (setf (gethash :end nodes) point)))))))
      (loop for row-idx = 0 then (1+ row-idx)
            for row in (split "\\n" input)
            do (import-row row-idx row)))
    (loop for point being the hash-values in nodes
          do (setf (gethash point edges) (get-neighbors point nodes)))
    (list nodes edges)))

(defun build-data (&optional input)
  (read-day-input #'parse-graph :whole t :input input))

(defun find-shortest-paths (nodes edges source)
  (let ((distance (make-hash-table :test #'equal))
        (previous (make-hash-table :test #'equal))
        (to-visit nil))
    (loop for node being the hash-values in nodes
          do (setf (gethash node distance) most-positive-fixnum
                   (gethash node previous) nil)
             (push node to-visit))
    (setf (gethash source distance) 0)

    (flet ((closest-node ()
             (loop with closest = (first to-visit)
                   for item in (rest to-visit)
                   when (< (gethash item distance) (gethash closest distance))
                     do (setf closest item)
                   finally (return closest))))
      (loop for node = (closest-node) while node
            do (removef to-visit node :test #'equal)
               (dolist (neighbor (gethash node edges))
                 (when (member neighbor to-visit :test #'equal)
                   (let ((alternate (1+ (gethash node distance))))
                     (when (< alternate (gethash neighbor distance))
                       (setf (gethash neighbor distance) alternate
                             (gethash neighbor previous) node)))))))

    (values distance previous)))

(defun part-1 (&optional (data (build-data)))
  (destructuring-bind (nodes edges) data
    (let ((destination (gethash :end nodes))
          (distances (find-shortest-paths nodes edges (gethash :start nodes))))
      (values (gethash destination distances) distances))))

(defun part-2 (&optional (data (build-data)))
  (destructuring-bind (nodes edges) data
    (let ((destination (gethash :end nodes))
          (origins (loop for point being the hash-values in nodes
                         when (= (char-code #\a) (height point)) collect point)))
      (format t "Origins to test: ~D~%" (length origins))
      (loop for i = 1 then (1+ i)
            for origin in origins
            for distances = (find-shortest-paths nodes edges origin)
            do (format t "Searched ~d/~d~%" i (length origins))
            minimizing (or (gethash destination distances) most-positive-fixnum) into min
            finally (return min)))))
