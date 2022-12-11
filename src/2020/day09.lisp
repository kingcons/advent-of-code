(mgl-pax:define-package :aoc.2020.09
  (:nicknames :2020.09)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :screamer #:/=v #:=v #:+v #:>v #:andv))

(in-package :2020.09)

(defsummary (:title "Encoding Error")
  "**Part 1** - Find vulnerable number"
  (find-invalid-number function)
  "**Part 2** - Break the encryption"
  (find-invalid-slice function))

(defvar *width* 25)

(defun build-data (&optional input)
  (coerce (read-day-input #'parse-integer :input input) 'vector))

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

(defun part-1 (&optional (data (build-data)))
  (find-invalid-number data))

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

(defun part-2 (&optional (data (build-data)))
  (find-invalid-slice data))
