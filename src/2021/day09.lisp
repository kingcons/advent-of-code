(mgl-pax:define-package :aoc.2021.09
  (:nicknames :2021.09)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :alexandria #:define-constant))

(in-package :2021.09)

(defsummary (:title "Smoke Basin")
  "**Part 1** - Follow the Smoke"
  (sum-risk-levels function)
  "**Part 2** - Dodge the Basins"
  (find-largest-basins function))

(defun get-bounds (grid)
  (etypecase grid
    (list (list (length grid) (length (first grid))))
    (array (array-dimensions grid))))

(defun do-grid (grid function)
  "Iterate over a 2 dimensional GRID calling FUNCTION with row, col, item for each entry."
  (let ((bounds (get-bounds grid)))
    (dotimes (row (first bounds))
      (dotimes (col (second bounds))
        (funcall function row col (aref grid row col))))))

(defun parse-grid (data)
  (let ((grid (make-array (get-bounds data) :initial-contents data)))
    (do-grid grid
      (lambda (row col item)
        (setf (aref grid row col) (- (char-code item) 48))))
    grid))

(define-constant +neighbors+
    '((0 1) (0 -1) (1 0) (-1 0)) :test #'equal)

(defun get-neighbors (grid row col &optional coords)
  (flet ((valid-neighbor? (x)
           (array-in-bounds-p grid (+ row (first x)) (+ col (second x))))
         (get-coords (x)
           (list (+ row (first x)) (+ col (second x))))
         (get-neighbor (x)
           (aref grid (+ row (first x)) (+ col (second x)))))
    (let ((eligible (remove-if-not #'valid-neighbor? +neighbors+)))
      (if coords
          (mapcar #'get-coords eligible)
          (mapcar #'get-neighbor eligible)))))

(defun lowpoint? (grid row col item)
  (let ((neighbors (get-neighbors grid row col)))
    (< item (apply 'min neighbors))))

(defun find-lowpoints (grid &optional coords)
  "Iterate over the entries in GRID collecting each lowpoint according to LOWPOINT?.
If COORDS is non-nil, collect the (ROW COL) position instead of the value."
  (let (lowpoints)
    (do-grid grid
      (lambda (row col item)
        (when (lowpoint? grid row col item)
          (push (if coords (list row col) item) lowpoints))))
    lowpoints))

(defun sum-risk-levels (grid)
  (reduce #'+ (mapcar #'1+ (find-lowpoints grid))))

(defun part-1 ()
  (let ((data (read-day-input #'parse-grid :whole t)))
    (sum-risk-levels data)))

(defun find-basin-size (grid row col)
  (let ((visited (make-hash-table :test #'equal))
        (to-visit `((,row ,col))))
    (labels ((completed? (x)
               (or (gethash x visited)
                   (= 9 (aref grid (first x) (second x)))))
             (unexplored (x)
               (let ((neighbors (get-neighbors grid (first x) (second x) t)))
                 (remove-if #'completed? neighbors))))
      (loop while to-visit
            do (let ((current (pop to-visit)))
                 (setf to-visit (nconc to-visit (unexplored current))
                       (gethash current visited) t))))
    (hash-table-count visited)))

(defun find-basins (grid)
  (loop for (row col) in (find-lowpoints grid t)
        collect (find-basin-size grid row col)))

(defun find-largest-basins (grid)
  (let ((basins (find-basins grid)))
    (reduce #'* (subseq (sort basins #'>) 0 3))))

(defun part-2 ()
  (let ((data (read-day-input #'parse-grid :whole t)))
    (find-largest-basins data)))
