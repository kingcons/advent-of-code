(mgl-pax:define-package :aoc.2020.05
  (:nicknames :2020.05)
  (:use :cl :aoc.util :mgl-pax)
  (:export #:decode #:highest-seat-id))

(in-package :2020.05)

(defsection @2020.05 (:title "Binary Boarding")
  (@part-1 section)
  (highest-seat-id function)
  (@part-2 section)
  (find-open-seat function))

(defsection @part-1 (:title "Binary Seat Encoding"))

(defun decode-bits (string &key (ones #\B))
  (loop with sum = 0
        for char across string
        for index = (length string) then (1- index)
        when (char= char ones)
          do (incf sum (expt 2 (1- index)))
        finally (return sum)))

(defun decode (boarding-pass)
  (let ((row (decode-bits (subseq boarding-pass 0 7)))
        (seat (decode-bits (subseq boarding-pass 7) :ones #\R)))
    (values row seat)))

(defun seat-id (pass)
  (multiple-value-bind (row seat) (decode pass)
    (+ (* row 8) seat)))

(defun highest-seat-id (passes)
  (loop for pass in passes maximizing (seat-id pass)))

(defun part-1 ()
  (let ((passes (read-day-input #'identity)))
    (summarize (highest-seat-id passes))))

(defsection @part-2 (:title "Find the unoccupied seat"))

(defun find-open-seat (passes)
  (let ((table (make-hash-table)))
    (dolist (pass passes)
      (let ((seat (seat-id pass)))
        (setf (gethash seat table) seat)))
    (loop for seat-id = 0 then (1+ seat-id)
          when (and (gethash (1- seat-id) table)
                    (not (gethash seat-id table))
                    (gethash (1+ seat-id) table))
            do (return-from find-open-seat seat-id))))

(defun part-2 ()
  (let ((passes (read-day-input #'identity)))
    (summarize (find-open-seat passes))))
