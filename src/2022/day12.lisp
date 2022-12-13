(mgl-pax:define-package :aoc.2022.12
  (:nicknames :2022.12)
  (:use :cl :mgl-pax :aoc.util)
  (:import-from :alexandria
                #:define-constant
                #:removef)
  (:import-from :cl-ppcre #:split)
  (:import-from :serapeum
                #:deq
                #:enq
                #:op
                #:queue))

(in-package :2022.12)

(defsummary (:title "Hill Climbing Algorithm")
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

(defun ye-olde-bfs (edges root goal-fn)
  (let ((distance (make-hash-table :test #'equal))
        (to-visit (queue root)))
    (setf (gethash root distance) 0)
    (loop for node = (deq to-visit) while node
          when (funcall goal-fn node)
            return (gethash node distance)
          do (dolist (edge (gethash node edges))
               (unless (gethash edge distance)
                 (setf (gethash edge distance) (1+ (gethash node distance)))
                 (enq edge to-visit))))))

(defun part-1 (&optional (data (build-data)))
  (destructuring-bind (nodes edges) data
    (let ((destination (gethash :end nodes)))
      (ye-olde-bfs edges (gethash :start nodes) (op (eql _ destination))))))

(defun part-2 (&optional (data (build-data)))
  (destructuring-bind (nodes edges) data
    (let ((destination (gethash :end nodes))
          (origins (loop for point being the hash-values in nodes
                         when (= (char-code #\a) (height point))
                           collect point)))
      (loop for i = 1 then (1+ i)
            for origin in origins
            for distances = (ye-olde-bfs edges origin (op (eql _ destination)))
            minimizing (or distances most-positive-fixnum) into min
            finally (return min)))))

;; (defun part-2 (&optional (data (build-data)))
;;   (destructuring-bind (nodes edges) data
;;     (let ((destination (gethash :end nodes))
;;           (origins (loop for point being the hash-values in nodes
;;                          when (= (char-code #\a) (height point))
;;                            collect point)))
;;       (ye-olde-bfs edges destination (op (member _ origins))))))
