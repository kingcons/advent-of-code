(defpackage :advent2020.day-09
  (:nicknames :day-09)
  (:use :cl)
  (:import-from :advent2020.util #:read-day-input)
  (:import-from :alexandria #:when-let)
  (:import-from :screamer #:/=v #:=v #:+v #:>v #:andv)
  (:export #:find-invalid-number #:find-invalid-range #:*width*))

(in-package :day-09)

(defvar *width* 25)

(defun a-previous-number (data index)
  (screamer:a-member-ofv (subseq data (- index *width*) index)))

(defun valid? (data index)
  (let* ((x (a-previous-number data index))
         (y (a-previous-number data index))
         (sum (+v x y)))
    (andv (=v sum (nth index data))
          (/=v x y))))

(defun find-invalid-number (data)
  (loop for i from *width* upto (length data)
        while (valid? data i)
        finally (return (values (nth i data) i))))

(defun part-1 ()
  (let ((data (read-day-input 9 #'parse-integer)))
    (find-invalid-number data)))

(defun find-weakness (data target index)
  (screamer:one-value
      (let* ((start (screamer:an-integer-between 0 index))
             (finish (screamer:an-integer-between start index))
             (range (subseq data start finish)))
        (screamer:assert! (=v (reduce #'+ range) target))
        (+ (apply #'min range) (apply #'max range)))))

(defun find-invalid-range (data)
  (multiple-value-bind (target index) (find-invalid-number data)
    (find-weakness data target index)))

(defun part-2 ()
  (let ((data (read-day-input 9 #'parse-integer)))
    (find-invalid-range data)))
