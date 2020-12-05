(defpackage :advent2020.util
  (:use :cl)
  (:export #:read-day-input))

(in-package :advent2020.util)

(defmacro read-day-input (day item-parser &key (separator "\\n"))
  (let ((filename (format nil "src/day~2,'0d-input.dat" day)))
    `(arrows:->> ,filename
       (asdf:system-relative-pathname :advent2020)
       (alexandria:read-file-into-string)
       (cl-ppcre:split ,separator)
       (mapcar ,item-parser))))
