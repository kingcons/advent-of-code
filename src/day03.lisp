(defpackage :advent2020.day-03
  (:nicknames :day-03)
  (:use :cl)
  (:import-from :advent2020.util #:read-day-input)
  (:import-from :split-sequence #:split-sequence)
  (:export #:count-trees #:count-collisions))

(in-package :day-03)

(defun parse-terrain (row)
  "Represent terrain as a circular list extending infinitely."
  (let ((items (coerce row 'list)))
    (setf (cdr (last items)) items)
    items))

(defun collision? (terrain column)
  (char= #\# (nth column terrain)))

(defun count-trees (map &key (down 1) (right 3))
  (loop
    for row = 0 then (+ row down)
    for column = 0 then (+ column right)
    for terrain = (nth row map)
    while terrain
    counting (collision? terrain column) into total
    finally (return total)))

(defun count-collisions (map)
  (loop
    for (down right) in '((1 1) (1 3) (1 5) (1 7) (2 1))
    collecting (count-trees map :down down :right right) into counts
    finally (return (reduce #'* counts))))

(defun part-1 ()
  (let ((map (read-day-input 3 #'parse-terrain)))
    (count-trees map)))

(defun part-2 ()
  (let ((map (read-day-input 3 #'parse-terrain)))
    (count-collisions map)))
