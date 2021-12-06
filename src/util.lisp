(defpackage :aoc.util
  (:use :cl)
  (:export #:read-day-input #:summarize))

(in-package :aoc.util)

(defmacro read-day-input (item-parser &key (separator "\\n"))
  "Load the input data for the current day based on the name of *PACKAGE*, splitting
the file based on the value of SEPARATOR and processing each string with ITEM-PARSER."
  (cl-ppcre:register-groups-bind (year day)
      ("(\\d{4})\.(\\d{2})" (package-name *package*))
    `(arrows:->> (format nil "src/~d/day~d.dat" ,year ,day)
                 (asdf:system-relative-pathname :advent)
                 (alexandria:read-file-into-string)
                 (cl-ppcre:split ,separator)
                 (mapcar ,item-parser))))

(defmacro summarize (form)
  "Measure the real time to execute FORM and return a string showing the result
and the wall clock execution time."
  (let ((old-bytes (gensym))
        (new-bytes (gensym))
        (useconds (gensym))
        (result (gensym))
        (start (gensym))
	(end (gensym)))
    `(let* ((,old-bytes (sb-ext:get-bytes-consed))
            (,start (get-internal-real-time))
            (,result ,form)
            (,end (get-internal-real-time))
            (,new-bytes (sb-ext:get-bytes-consed))
            (,useconds (/ (- ,end ,start) internal-time-units-per-second)))
       (format nil "> Time: ~7,3fms  Memory: ~7:dkb  Answer: ~10T~a~%"
               (* 1000 ,useconds) (floor (- ,new-bytes ,old-bytes) 1024) ,result))))
