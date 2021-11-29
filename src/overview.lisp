(defpackage :aoc.overview
  (:use :cl :mgl-pax)
  (:export @overview))

(in-package :aoc.overview)

(defun format-day (day)
  (let* ((package (find-package day))
         (main-section (symbol-value (find-symbol (format nil "@~A" day) package)))
         (section-1 (symbol-value (find-symbol "@PART-1" package)))
         (section-2 (symbol-value (find-symbol "@PART-2" package)))
         (summary-1 (funcall (find-symbol "PART-1" package)))
         (summary-2 (funcall (find-symbol "PART-2" package))))
    (format nil "##### ~A Day ~A: ~A
  * Part 1: ~A~%~A
  * Part 2: ~A~%~A"
            (subseq (symbol-name day) 0 4)
            (subseq (symbol-name day) 5)
            (section-title main-section)
            (section-title section-1)
            summary-1
            (section-title section-2)
            summary-2)))

;;; The purpose of the below code snippet is to allow easily generating markdown for inclusion
;;; in the @overview section using emacs' sly-eval-print-last-expression. There may be a way
;;; to do something similar with cl-transcribe but it seems pretty involved and I would also
;;; expect it to go against its design intent since perf measurements will change run to run.

#+nil
(let ((days-attempted '(:2019.01
                        ;:2020.01 :2020.02 :2020.03 :2020.04 :2020.05 :2020.06 :2020.07 :2020.08
                        )))
  (apply 'concatenate 'string
         (mapcar #'format-day days-attempted)))

(defsection @overview (:title "Overview")
  "##### 2019 Day 01: The Tyranny of the Rocket Equation
  * Part 1: Fuel for Modules
> Time:   0.002ms  Answer:  3282935

  * Part 2: Fuel for Fuel
> Time:   0.021ms  Answer:  4921542
")
