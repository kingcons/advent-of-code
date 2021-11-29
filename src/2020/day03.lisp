(mgl-pax:define-package :aoc.2020.03
  (:nicknames :2020.03)
  (:use :cl :aoc.util :mgl-pax))

(in-package :2020.03)

(defsection @2020.03 (:title "Toboggan Trajectory")
  (@part-1 section)
  (count-trees function)
  (@part-2 section)
  (count-collisions function))

(defsection @part-1 (:title "Count tree collisions"))

(defun count-trees (map &key (down 1) (right 3))
  (loop
    for terrain on map by (alexandria:curry #'nthcdr down)
    for column = 0 then (+ column right)
    while (first terrain)
    counting (collision? (first terrain) column) into total
    finally (return total)))

(defun part-1 ()
  (let ((map (read-day-input #'identity)))
    (summarize (count-trees map))))

(defsection @part-2 (:title "Count multiple slopes"))

(defun collision? (terrain column)
  (char= #\# (char terrain (mod column (length terrain)))))

(defun count-collisions (map)
  (loop
    for (down right) in '((1 1) (1 3) (1 5) (1 7) (2 1))
    collecting (count-trees map :down down :right right) into counts
    finally (return (reduce #'* counts))))

(defun part-2 ()
  (let ((map (read-day-input #'identity)))
    (summarize (count-collisions map))))
