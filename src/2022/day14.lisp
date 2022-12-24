(mgl-pax:define-package :aoc.2022.14
  (:nicknames :2022.14)
  (:use :cl :mgl-pax :aoc.util :aoc.parsers :esrap)
  (:import-from :alexandria
                #:hash-table-keys
                #:hash-table-values
                #:lastcar)
  (:import-from :serapeum
                #:op
                #:partial))

(in-package :2022.14)

(defsummary (:title "Regolith Reservoir")
  "**Part 1** - "

  "**Part 2** - ")

(defstruct (point (:type list)) x y)

(defrule point (and integer #\, integer (? " -> "))
  (:lambda (point) (list (first point) (third point))))

(defrule cave-path (+ point))

(defun build-data (&optional input)
  (read-day-input (partial #'parse 'cave-path) :input input))

(defun find-range (point1 point2)
  (let ((direction (if (= (point-x point1) (point-x point2)) :vertical :horizontal)))
    (case direction
      (:horizontal (values direction (sort (list (point-x point1) (point-x point2)) #'<)))
      (:vertical (values direction (sort (list (point-y point1) (point-y point2)) #'<))))))

(defun find-bounds (cave floor-mod)
  (loop for point in (hash-table-keys cave)
        minimizing (point-x point) into min-x
        maximizing (point-x point) into max-x
        minimizing (point-y point) into min-y
        maximizing (+ (point-y point) floor-mod) into max-y
        finally (return (list min-x max-x min-y max-y))))

(defun build-cave (data &key (floor-mod 0))
  (let ((cave (make-hash-table :test #'equal)))
    (flet ((add-segment (point1 point2)
             (multiple-value-bind (direction range) (find-range point1 point2)
               (loop for i from (first range) upto (second range)
                     do (case direction
                          (:horizontal (setf (gethash (list i (point-y point1)) cave) :rock))
                          (:vertical (setf (gethash (list (point-x point1) i) cave) :rock)))))))
      (dolist (cave-path data)
        (loop for (point1 point2) on cave-path by #'rest
              while point2 do (add-segment point1 point2)))
      (setf (gethash :bounds cave) (find-bounds cave floor-mod)))
    cave))

(defun next-position (point cave)
  (let ((options (destructuring-bind (x y) point
                   (list (make-point :x x      :y (1+ y))
                         (make-point :x (1- x) :y (1+ y))
                         (make-point :x (1+ x) :y (1+ y))))))
    (find-if-not (op (gethash _ cave)) options)))

(defun place-sand (cave &key (abyss? t))
  (loop with sand = (make-point :x 500 :y 0)
        with bottom = (lastcar (gethash :bounds cave))
        do (let ((next (next-position sand cave)))
             (cond ((null next)
                    (return (setf (gethash sand cave) :rest)))
                   ((and abyss? (> (point-y next) bottom))
                    (return :abyss))
                   ((and (null abyss?) (= (point-y next) bottom))
                    (return (setf (gethash sand cave) :rest)))
                   (t (setf sand next))))))

(defun count-sand-grains (cave &key (abyss? t))
  (loop for result = (place-sand cave :abyss? abyss?)
        until (or (eql result :abyss) (gethash '(500 0) cave)))
  (count :rest (hash-table-values cave)))

(defun part-1 (&optional (data (build-data)))
  (count-sand-grains (build-cave data)))

(defun render (cave &key (floor :floor))
  (destructuring-bind (min-x max-x min-y max-y) (gethash :bounds cave)
    (declare (ignore min-y))
    (dotimes (y (1+ max-y))
      (loop for i from (- min-x 8) upto (+ max-x 8)
            do (let ((glyph (case (gethash (list i y) cave)
                              (:rest "o")
                              (:rock "#")
                              (t "."))))
                 (if (and (= y max-y) (eql floor :floor))
                     (format t "#")
                     (format t "~A" glyph))))
      (format t "~%"))
    (count :rest (hash-table-values cave))))

(defun part-2 (&optional (data (build-data)))
  (let ((cave (build-cave data :floor-mod 2)))
    (count-sand-grains cave :abyss? nil)))
