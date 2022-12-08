(mgl-pax:define-package :aoc.2022.08
  (:nicknames :2022.08)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :cl-ppcre #:split)
  (:import-from :serapeum #:op))

(in-package :2022.08)

(defsummary (:title "Treetop Tree House")
  "**Part 1** - "

  "**Part 2** - ")

(defun make-cardinal (row col limit direction data)
  (ecase direction
    (:south (loop for i from (1+ row) below limit collecting (aref data i col)))
    (:north (reverse (loop for i below row collecting (aref data i col))))
    (:east (loop for i from (1+ col) below limit collecting (aref data row i)))
    (:west (reverse (loop for i below col collecting (aref data row i))))))

(defun check-cardinals? (row col data limit)
  (let ((value (aref data row col)))
    (flet ((tallest? (other)
             (> value other)))
      (or (every #'tallest? (make-cardinal row col limit :south data))
          (every #'tallest? (make-cardinal row col limit :north data))
          (every #'tallest? (make-cardinal row col limit :east data))
          (every #'tallest? (make-cardinal row col limit :west data))))))

(defun visible? (row col data limit)
  (cond ((zerop row) t)
        ((zerop col) t)
        ((= limit row) t)
        ((= limit col) t)
        ((check-cardinals? row col data limit) t)
        (t nil)))

(defun count-visible? (data)
  (loop with width = (array-dimension data 0)
        for row below width
        sum (loop for col below width
                  count (visible? row col data width))))

(defun parse-row (row)
  (mapcar #'parse-integer (split "" row)))

(defun build-grid (&optional input)
  (let ((data (read-day-input #'parse-row :input input)))
    (make-array (list (length data) (length data)) :initial-contents data)))

(defun part-1 ()
  (let ((grid (build-grid)))
    (count-visible? grid)))

(defun scenic-score (row col limit data)
  (let ((value (aref data row col)))
    (flet ((view-distance (trees)
             (let ((blocker (position-if (op (>= _ value)) trees)))
               (or (and blocker (1+ blocker))
                   (length trees)))))
      (let ((results (mapcar (op (view-distance (make-cardinal row col limit _ data)))
                             '(:north :west :east :south))))
        (values (apply '* results) results)))))

(defun max-score (data)
  (loop with width = (array-dimension data 0)
        for row below width
        maximize (loop for col below width
                       maximize (scenic-score row col width data))))

(defun part-2 ()
  (let ((grid (build-grid)))
    (max-score grid)))
