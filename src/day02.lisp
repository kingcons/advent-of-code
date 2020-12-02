(defpackage :advent2020.day-02
  (:nicknames :day-02)
  (:use :cl)
  (:import-from :advent2020.util #:read-day-input)
  (:export #:count-valid #:count-xor-valid))

(in-package :day-02)

(defun parse-password (row)
  (cl-ppcre:register-groups-bind ((#'parse-integer min max) letter password)
      ("(\\d+)-(\\d+) (\\w): (\\w+)" row)
    (list min max letter password)))

(defun count-valid (items)
  (loop for (min max letter password) in items
        counting (let ((count (count letter password :test #'string=)))
                   (<= min count max))))

(defun count-xor-valid (items)
  (flet ((letter-at? (letter string index)
           (count letter string :start (1- index) :end index :test #'string=)))
    (loop for (min max letter password) in items
          counting (plusp (logxor (letter-at? letter password min)
                                  (letter-at? letter password max))))))

(defun part-1 ()
  (let ((items (read-day-input 2 #'parse-password)))
    (count-valid items)))

(defun part-2 ()
  (let ((items (read-day-input 2 #'parse-password)))
    (count-xor-valid items)))
