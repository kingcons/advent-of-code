(defpackage :advent2020.day-03
  (:nicknames :day-03)
  (:use :cl)
  (:import-from :advent2020.util #:read-day-input)
  (:export #:count-trees #:count-collisions))

(in-package :day-03)

(declaim (inline collision?))
(defun collision? (terrain column)
  (char= #\# (char terrain column)))

(defun count-trees (map &key (down 1) (right 3))
  (loop
    with line-width = (length (first map))
    for terrain on map by (alexandria:curry #'nthcdr down)
    for column = 0 then (mod (+ column right) line-width)
    while (first terrain)
    counting (collision? (first terrain) column) into total
    finally (return total)))

(defun count-collisions (map)
  (loop
    for (down right) in '((1 1) (1 3) (1 5) (1 7) (2 1))
    collecting (count-trees map :down down :right right) into counts
    finally (return (reduce #'* counts))))

(defun part-1 ()
  (let ((map (read-day-input 3 #'identity)))
    (count-trees map)))

(defun part-2 ()
  (let ((map (read-day-input 3 #'identity)))
    (count-collisions map)))
