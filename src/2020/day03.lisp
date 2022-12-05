(mgl-pax:define-package :aoc.2020.03
  (:nicknames :2020.03)
  (:use :cl :aoc.util :mgl-pax))

(in-package :2020.03)

(defsummary (:title "Toboggan Trajectory")
  "**Part 1** - Count tree collisions"
  (count-trees function)
  "**Part 2** - Count multiple slopes"
  (count-collisions function))

(defun count-trees (map &key (down 1) (right 3))
  (loop
    for terrain on map by (alexandria:curry #'nthcdr down)
    for column = 0 then (+ column right)
    while (first terrain)
    counting (collision? (first terrain) column) into total
    finally (return total)))

(defun part-1 ()
  (let ((map (read-day-input #'identity)))
    (count-trees map)))

(defun collision? (terrain column)
  (char= #\# (char terrain (mod column (length terrain)))))

(defun count-collisions (map)
  (loop
    for (down right) in '((1 1) (1 3) (1 5) (1 7) (2 1))
    collecting (count-trees map :down down :right right) into counts
    finally (return (reduce #'* counts))))

(defun part-2 ()
  (let ((map (read-day-input #'identity)))
    (count-collisions map)))
