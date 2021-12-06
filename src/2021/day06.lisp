(mgl-pax:define-package :aoc.2021.06
  (:nicknames :2021.06)
  (:use :cl :aoc.util :mgl-pax))

(in-package :aoc.2021.06)

(defsection @2021.06 (:title "Lanternfish")
  (@part-1 section)
  (@part-2 section))

(defsection @part-1 (:title "That's a big school"))

(defstruct school
  (days 0)
  (counts (make-array 9 :element-type 'fixnum)))

(defun parse-school (fishlist)
  (let ((counts (make-array 9 :element-type 'fixnum))
        (timers (mapcar #'parse-integer (cl-ppcre:split "," fishlist))))
    (dolist (timer timers)
      (incf (aref counts timer)))
    (make-school :counts counts)))

(defun tick (school)
  (with-slots (counts) school
    (declare (type (simple-array fixnum) counts))
    (alexandria:rotate counts -1)
    (let ((newborns (aref counts 8))
          (old-gen (aref counts 6)))
      (setf (aref counts 6) (+ old-gen newborns)))))

(defun estimate-population (school days)
  (dotimes (i days)
    (tick school))
  (reduce #'+ (school-counts school)))

(defun part-1 ()
  (let ((data (first (read-day-input #'parse-school))))
    (summarize (estimate-population data 80))))

(defsection @part-2 (:title "Uh Oh"))

(defun part-2 ()
  (let ((data (first (read-day-input #'parse-school))))
    (summarize (estimate-population data 256))))
