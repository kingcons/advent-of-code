(defpackage :aoc.util
  (:use :cl)
  (:export #:read-day-input))

(in-package :aoc.util)

(defmacro read-day-input (item-parser &key (separator "\\n"))
  (cl-ppcre:register-groups-bind (year day)
      ("(\\d{4})\.(\\d{2})" (package-name *package*))
    `(arrows:->> (format nil "src/~d/day~d.dat" ,year ,day)
                 (asdf:system-relative-pathname :advent)
                 (alexandria:read-file-into-string)
                 (cl-ppcre:split ,separator)
                 (mapcar ,item-parser))))
