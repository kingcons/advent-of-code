(defpackage :aoc.2020.09
  (:nicknames :2020.09)
  (:use :cl :aoc.util)
  (:import-from :screamer #:/=v #:=v #:+v #:>v #:andv)
  (:export #:find-invalid-number #:find-invalid-slice #:*width*))

(in-package :2020.09)

(defvar *width* 25)

(defun a-previous-number (data index)
  (screamer:a-member-ofv (subseq data (- index *width*) index)))

(defun valid? (data index)
  "Can any 2 previous numbers in DATA sum to the number at INDEX?"
  (let* ((x (a-previous-number data index))
         (y (a-previous-number data index))
         (sum (+v x y)))
    (andv (/=v x y)
          (=v sum (aref data index)))))

(defun find-invalid-number (data)
  (loop for i from *width* upto (length data)
        while (valid? data i)
        finally (return (values (aref data i) i))))

(defun part-1 ()
  (let ((data (coerce (read-day-input #'parse-integer) 'vector)))
    (find-invalid-number data)))

(defun find-weakness (data target index)
  "Find a range of numbers below INDEX in DATA that sum to TARGET.
   Sum the smallest and largest numbers in that slice."
  (screamer:one-value
      (let* ((start (screamer:an-integer-between 0 index))
             (finish (screamer:an-integer-between start index))
             (slice (make-array (- finish start)
                                :displaced-to data
                                :displaced-index-offset start)))
        (screamer:assert! (=v (reduce #'+ slice) target))
        (let ((slice (coerce slice 'list)))
          (+ (apply #'min slice) (apply #'max slice))))))

(defun find-invalid-slice (data)
  (multiple-value-bind (target index) (find-invalid-number data)
    (find-weakness data target index)))

(defun part-2 ()
  (let ((data (coerce (read-day-input #'parse-integer) 'vector)))
    (find-invalid-slice data)))
