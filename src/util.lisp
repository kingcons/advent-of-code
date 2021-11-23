(defpackage :aoc.util
  (:use :cl)
  (:export #:read-day-input))

(in-package :aoc.util)

(defmacro read-day-input (item-parser &key (separator "\\n"))
  (let* ((base-pathname (or *load-pathname* *compile-file-pathname*))
         (filename (make-pathname :defaults base-pathname :type "dat")))
    `(arrows:->> ,filename
                 (asdf:system-relative-pathname :advent)
                 (alexandria:read-file-into-string)
                 (cl-ppcre:split ,separator)
                 (mapcar ,item-parser))))
