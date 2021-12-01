(mgl-pax:define-package :aoc.2021.01
  (:nicknames :2021.01)
  (:use :cl :aoc.util :mgl-pax))

(in-package :2021.01)

(defsection @2021.01 (:title "Sonar Sweep")
  (@part-1 section)
  (count-depths function)
  (@part-2 section)
  (count-depths-sliding function))

(defsection @part-1 (:title "Into the Depths")
  "For part 1, we'll just be checking how often a depth reading
is increasing from the previous measurement. Pretty straightforward.")

(defun count-depths (sonar-readings)
  (loop for (previous depth) on sonar-readings
        while depth
        counting (> depth previous)))

(defun part-1 ()
  (let ((data (read-day-input #'parse-integer)))
    (summarize (count-depths data))))

(defsection @part-2 (:title "Average the Depths")
  "Part 2 extends our initial solution by averaging the depth readings
in a sliding window three at a time. I'm still using a straightforward
loop but the partitioning of the list is ugly. Two options for improvement are:

1. Solve the problem in two passes, first generating sums then counting increases.
2. Factor out the size of the window, either via callback or some other means.")

(defun count-depths-sliding (sonar-readings)
  (loop with previous = (reduce #'+ (subseq sonar-readings 0 3))
        for (first second third) on (subseq sonar-readings 1)
        while third
        for sum = (+ first second third)
        count (> sum previous)
        do (setf previous sum)))

(defun part-2 ()
  (let ((data (read-day-input #'parse-integer)))
    (summarize (count-depths-sliding data))))
