(mgl-pax:define-package :aoc.2021.09
  (:nicknames :2021.09)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :aoc.parsers #:parse-grid)
  (:import-from :alexandria #:define-constant))

(in-package :2021.09)

(defsummary (:title "Smoke Basin")
  "**Part 1** - Follow the Smoke"
  (sum-risk-levels function)
  "**Part 2** - Dodge the Basins"
  (find-largest-basins function))

(define-constant +neighbors+
    '((0 1) (0 -1) (1 0) (-1 0)) :test #'equal)

(defun build-data (&optional input)
  (flet ((build-grid (input)
           (parse-grid input :transform (lambda (x) (- (char-code x) 48)))))
    (read-day-input #'build-grid :whole t :input input)))

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

(defun part-1 (&optional (data (build-data)))
  (sum-risk-levels data))

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

(defun part-2 (&optional (data (build-data)))
  (find-largest-basins data))
