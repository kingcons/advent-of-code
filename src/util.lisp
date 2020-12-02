(defpackage :advent2020.util
  (:use :cl)
  (:export #:read-day-input))

(defmacro read-day-input (day item-parser)
  (let ((filename (format nil "src/day~2,'0d-input.dat" day)))
    `(arrows:->> ,filename
       (asdf:system-relative-pathname :advent2020)
       (alexandria:read-file-into-string)
       (split-sequence:split-sequence #\Newline)
       (butlast)
       (mapcar ,item-parser))))
