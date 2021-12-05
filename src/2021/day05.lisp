(mgl-pax:define-package :aoc.2021.05
  (:nicknames :2021.05)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :alexandria :hash-table-values))

(in-package :2021.05)

(defsection @2021.05 (:title "Hydrothermal Venture")
  (@part-1 section)
  (@part-2 section))

(defsection @part-1 (:title "Overlapping Vents"))

(defstruct point
  (x-pos 0 :type fixnum)
  (y-pos 0 :type fixnum))

(defun parse-point (line)
  (cl-ppcre:register-groups-bind ((#'parse-integer x1 y1 x2 y2))
      ("(\\d+),(\\d+)\\s+->\\s+(\\d+),(\\d+)" line)
    (list (make-point :x-pos x1 :y-pos y1)
          (make-point :x-pos x2 :y-pos y2))))

(defun coordinate-match? (segment)
  (destructuring-bind (p1 p2) segment
    (or (= (point-x-pos p1) (point-x-pos p2))
        (= (point-y-pos p1) (point-y-pos p2)))))

(defun build-range (n1 n2 length)
  (cond ((= n1 n2) (loop repeat (1+ length) collect n1))
        ((< n1 n2) (loop for i from n1 upto n2 collect i))
        ((> n1 n2) (loop for i from n1 downto n2 collect i))))

(defun list-points (segment)
  (destructuring-bind (p1 p2) segment
    (let ((length (max (abs (- (point-x-pos p1) (point-x-pos p2)))
                       (abs (- (point-y-pos p1) (point-y-pos p2))))))
      (mapcar #'list
              (build-range (point-x-pos p1) (point-x-pos p2) length)
              (build-range (point-y-pos p1) (point-y-pos p2) length)))))

(defun find-overlapping-vents (segments &key (skip-diagonal t))
  (let ((valid-segments (if skip-diagonal
                            (remove-if-not #'coordinate-match? segments)
                            segments))
        (grid (make-hash-table :test #'equal)))
    (loop for i = 0 then (1+ i)
          for segment in valid-segments
          do (dolist (point (list-points segment))
               (let ((seen (gethash point grid 0)))
                 (setf (gethash point grid) (1+ seen))))
          finally (return (count-if (lambda (x) (> x 1)) (hash-table-values grid))))))

(defun part-1 ()
  (let ((segments (read-day-input #'parse-point)))
    (time (find-overlapping-vents segments))))

(defsection @part-2 (:title "Diagonal Overlap"))

(defun part-2 ()
  (let ((segments (read-day-input #'parse-point)))
    (time (find-overlapping-vents segments :skip-diagonal nil))))
