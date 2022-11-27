(mgl-pax:define-package :aoc.2021.11
  (:nicknames :2021.11)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :alexandria #:define-constant #:hash-table-keys #:hash-table-values))

(in-package :2021.11)

(defsection @2021.11 (:title "Dumbo Octopuses")
  (@part-1 section)
  (@part-2 section))

(defsection @part-1 (:title ""))

(define-constant +adjacents+
  '((0 1) (0 -1) (1 0) (-1 0) (1 1) (-1 -1) (1 -1) (-1 1))
  :test #'equal)

(defun parse-grid (data)
  (flet ((ascii-digit-to-int (x)
           (- (char-code x) 48)))
    (let ((grid (make-hash-table :test #'equal)))
      (dotimes (row (length data))
        (dotimes (col (length (first data)))
          (setf (gethash (cons row col) grid)
                (ascii-digit-to-int (aref (nth row data) col)))))
      grid)))

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

(defun part-1 ()
  (let ((data (read-day-input #'parse-grid :whole t)))
    (summarize (count-flashes data 100))))

(defsection @part-2 (:title ""))

(defun step-until-n-flashes (grid n)
  (loop for i = 1 then (1+ i)
        for flashes = (tick grid)
        until (= flashes n)
        finally (return i)))

(defun part-2 ()
  (let ((data (read-day-input #'parse-grid :whole t)))
    (summarize (step-until-n-flashes data 100))))
