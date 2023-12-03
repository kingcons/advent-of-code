(mgl-pax:define-package :aoc.2023.03
  (:nicknames :2023.03)
  (:use :cl :mgl-pax :aoc.util :esrap))

(in-package :2023.03)

(defsummary (:title "Gear Ratios")
  "**Parsing**"

  "**Part 1**"

  "**Part 2**")

(defvar *line-width* nil)

(defrule engine-symbol (or #\@ #\# #\$ #\% #\& #\* #\- #\+ #\/ #\=)
  (:lambda (match esrap:&bounds start)
    (let ((newlines (floor start (1+ *line-width*))))
      (list match (- start newlines)))))

(defrule ignored-space (or (+ #\.) #\Newline)
  (:constant nil))

(defrule part-number (+ (character-ranges (#\0 #\9)))
  (:text t)
  (:lambda (match esrap:&bounds start end)
    (let ((newlines (floor start (1+ *line-width*))))
      (list (parse-integer match) (- start newlines) (- end newlines 1)))))

(defrule grid (+ (or ignored-space engine-symbol part-number))
  (:lambda (list)
    (remove nil list)))

(defun parts-map (input)
  (let ((*line-width* (position #\Newline input)))
    (loop with parts = (make-hash-table :test #'equalp)
          with markers = (make-hash-table)
          for (match . position) in (parse 'grid input)
          do (if (stringp match)
                 (setf (gethash (first position) markers) match)
                 (setf (gethash position parts) match))
          finally (return (list parts markers)))))

(defun build-data (&optional input)
  (read-day-input #'parts-map :input input :whole t))

(defun neighbors (start end)
  (loop for i from (1- start) to (1+ end)
        collect i
        collect (- i 10)
        collect (+ i 10)))

(defun connected? (markers start end)
  (loop for neighbor in (neighbors start end)
        thereis (gethash neighbor markers)))

(defun find-connected (parts markers)
  (loop for (start end) being the hash-keys in parts
        using (hash-value part-number)
        when (connected? markers start end)
          collect part-number))

(defun part-1 (&optional (data (build-data)))
  (destructuring-bind (parts markers) data
    (reduce #'+ (find-connected parts markers))))

(defun part-2 (&optional (data (build-data)))
  (bar data))
