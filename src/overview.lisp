(defpackage :aoc.overview
  (:use :cl :mgl-pax)
  (:export @overview))

(in-package :aoc.overview)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun format-day (day)
    (let* ((package (find-package day))
           (main-section (symbol-value (find-symbol (format nil "@~A" day) package)))
           (section-1 (symbol-value (find-symbol "@PART-1" package)))
           (section-2 (symbol-value (find-symbol "@PART-2" package))))
      (unless (and (find-symbol "PART-1" package)
                   (find-symbol "PART-2" package))
        (return-from format-day nil))
      (format nil "##### ~A Day ~A: ~A
  * Part 1: ~A~%~A
  * Part 2: ~A~%~A"
              (subseq (symbol-name day) 0 4)
              (subseq (symbol-name day) 5)
              (section-title main-section)
              (section-title section-1)
              (funcall (find-symbol "PART-1" package))
              (section-title section-2)
              (funcall (find-symbol "PART-2" package))))))

;;; The purpose of the below code snippet is to allow easily generating markdown for inclusion
;;; in the @overview section using emacs' sly-eval-print-last-expression. There may be a way
;;; to do something similar with cl-transcribe but it seems pretty involved and I would also
;;; expect it to go against its design intent since perf measurements will change run to run.

(defmacro generate-overview ()
  (flet ((build-package-name (pathname)
           (cl-ppcre:register-groups-bind (year day)
               ("(\\d+)/day(\\d+).lisp" (namestring pathname))
             (intern (concatenate 'string year "." day) :keyword))))
    (let* ((src-dir (asdf:system-relative-pathname :advent "src/"))
           (lisp-files (uiop:directory-files src-dir "*/*.lisp"))
           (days-attempted (mapcar #'build-package-name lisp-files))
           (formatted-overview
             (apply 'concatenate 'string
                    (mapcar #'format-day days-attempted))))
      `(defsection @overview (:title "Overview")
         ,formatted-overview))))

(generate-overview)
