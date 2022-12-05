(mgl-pax:define-package :aoc.2021.02
  (:nicknames :2021.02)
  (:use :cl :aoc.util :mgl-pax))

(in-package :2021.02)

(defsummary (:title "Dive!")
  "**Part 1** - Plotting the Course"
  (plot-course function)
  "**Part 2** - One Does Not Simply Dive"
  (plot-course-aim function))

(defun parse-navigation (line)
  (destructuring-bind (direction amount) (cl-ppcre:split " " line)
    (list (intern (string-upcase direction) :keyword)
          (parse-integer amount))))

(defun plot-course (plan)
  (loop with depth = 0 and horizontal = 0
        for (step amount) in plan
        do (ecase step
             (:forward (incf horizontal amount))
             (:up (decf depth amount))
             (:down (incf depth amount)))
        finally (return (* depth horizontal))))

(defun part-1 ()
  (let ((data (read-day-input #'parse-navigation)))
    (plot-course data)))

(defun plot-course-aim (plan)
  (loop with depth = 0 and aim = 0 and position = 0
        for (step amount) in plan
        do (ecase step
             (:down (incf aim amount))
             (:up (decf aim amount))
             (:forward
              (incf position amount)
              (incf depth (* aim amount))))
        finally (return (* depth position))))

(defun part-2 ()
  (let ((data (read-day-input #'parse-navigation)))
    (plot-course-aim data)))
