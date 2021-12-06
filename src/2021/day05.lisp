(mgl-pax:define-package :aoc.2021.05
  (:nicknames :2021.05)
  (:use :cl :aoc.util :mgl-pax))

(in-package :2021.05)

(defsection @2021.05 (:title "Hydrothermal Venture")
  (@part-1 section)
  (find-overlapping-vents function)
  (@part-2 section))

(defsection @part-1 (:title "Overlapping Vents"))

(defstruct segment
  x1 y1 x2 y2)

(defun parse-segment (line)
  (cl-ppcre:register-groups-bind ((#'parse-integer x1 y1 x2 y2))
      ("(\\d+),(\\d+)\\s+->\\s+(\\d+),(\\d+)" line)
    (make-segment :x1 x1 :y1 y1 :x2 x2 :y2 y2)))

(defun coordinate-match? (segment)
  (with-slots (x1 y1 x2 y2) segment
    (or (= x1 x2)
        (= y1 y2))))

(defun build-range (n1 n2 length)
  (cond ((= n1 n2) (loop repeat (1+ length) collect n1))
        ((< n1 n2) (loop for i from n1 upto n2 collect i))
        ((> n1 n2) (loop for i from n1 downto n2 collect i))))

(defun list-points (segment)
  (with-slots (x1 y1 x2 y2) segment
    (let ((length (max (abs (- x1 x2))
                       (abs (- y1 y2)))))
      (mapcar #'list
              (build-range x1 x2 length)
              (build-range y1 y2 length)))))

(defun find-overlapping-vents (segments)
  (let ((grid (make-hash-table :test #'equal))
        (overlaps 0))
    (loop for segment in segments
          do (dolist (point (list-points segment))
               (let ((seen (incf (gethash point grid 0))))
                 (when (= seen 2)
                   (incf overlaps))))
          finally (return (values overlaps grid)))))

(defun part-1 ()
  (let ((segments (read-day-input #'parse-segment)))
    (summarize (find-overlapping-vents (remove-if-not #'coordinate-match? segments)))))

(defsection @part-2 (:title "Diagonal Overlap"))

(defun part-2 ()
  (let ((segments (read-day-input #'parse-segment)))
    (summarize (find-overlapping-vents segments))))
