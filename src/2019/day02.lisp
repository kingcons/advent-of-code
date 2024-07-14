(mgl-pax:define-package :aoc.2019.02
  (:nicknames :2019.02)
  (:use :cl :mgl-pax :aoc.util)
  (:import-from :alexandria #:iota #:map-permutations))

(in-package :2019.02)

(defsummary (:title "")
  "**Parsing**"

  "**Part 1**"

  "**Part 2**")

(defun build-data (&optional input)
  (read-day-input #'parse-integer :input input :separator ","))

(defvar *memory* nil
  "An array representing global memory for an IntCode computer.")

(defun bin-op (operator x-idx y-idx dest)
  (let ((result (funcall operator (aref *memory* x-idx) (aref *memory* y-idx))))
    (setf (aref *memory* dest) result)))

(defun run-opcode (op x-index y-index dest)
  (case op
    (1 (bin-op #'+ x-index y-index dest))
    (2 (bin-op #'* x-index y-index dest))))

(defun run-intcode (data &optional boot-sector)
  (when boot-sector
    (init-memory boot-sector))
  (loop for (op x-idx y-idx dest) on data by #'cddddr
        until (= op 99)
        do (run-opcode op x-idx y-idx dest))
  (aref *memory* 0))

(defun init-memory (boot-sector)
  (setf (aref *memory* 1) (first boot-sector)
        (aref *memory* 2) (second boot-sector)))

(defun part-1 (&optional (data (build-data)))
  (let ((*memory* (coerce data 'vector)))
    (run-intcode data)))

(defun part-2 (&optional (data (build-data)))
  (flet ((test-boot-sector (values)
           (let* ((*memory* (coerce data 'vector))
                  (result (run-intcode data values)))
             (when (= result 19690720) ;; We landed on the moon!
               (return-from part-2 (+ (* 100 (first values)) (second values)))))))
    (map-permutations #'test-boot-sector (iota 100) :length 2)))
