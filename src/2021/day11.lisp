(mgl-pax:define-package :aoc.2021.11
  (:nicknames :2021.11)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :aoc.parsers #:parse-grid)
  (:import-from :alexandria
                #:define-constant
                #:hash-table-keys)
  (:import-from :serapeum #:op))

(in-package :2021.11)

(defsummary (:title "Dumbo Octopuses"))

(define-constant +adjacents+
  '((0 1) (0 -1) (1 0) (-1 0) (1 1) (-1 -1) (1 -1) (-1 1))
  :test #'equal)

(defun build-data (&optional input)
  (flet ((build-grid (input)
           (parse-grid input :container :hash
                             :transform (lambda (x row col)
                                          (declare (ignore row col))
                                          (- (char-code x) 48)))))
    (read-day-input #'build-grid :whole t :input input)))

(defun neighbors (position grid)
  (flet ((new-coord (x)
           (cons (+ (first x) (car position))
                 (+ (second x) (cdr position))))
         (valid? (x)
           (gethash x grid)))
    (let ((coordinates (mapcar #'new-coord +adjacents+)))
      (remove-if-not #'valid? coordinates))))

(defun tick (grid)
  (let ((flashed '()))
    (loop with to-process = (hash-table-keys grid)
          until (null to-process)
          do (let* ((current (pop to-process))
                    (new-value (incf (gethash current grid))))
               (when (= new-value 10)
                 (setf to-process (nconc to-process (neighbors current grid)))
                 (push current flashed))))
    (dolist (octopus flashed)
      (setf (gethash octopus grid) 0))
    (length flashed)))

(defun count-flashes (grid steps)
  (let ((count 0))
    (dotimes (i steps)
      (incf count (tick grid)))
    count))

(defun part-1 (&optional (data (build-data)))
  (count-flashes data 100))

(defun step-until-n-flashes (grid n)
  (loop for i = 1 then (1+ i)
        for flashes = (tick grid)
        until (= flashes n)
        finally (return i)))

(defun part-2 (&optional (data (build-data)))
  (step-until-n-flashes data 100))
