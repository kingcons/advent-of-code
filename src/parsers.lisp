(mgl-pax:define-package :aoc.parsers
  (:use :cl :mgl-pax :esrap)
  (:import-from :cl-ppcre #:split))

(in-package :aoc.parsers)

(defsection @aoc.parsers (:title "Parsing Utilities")
  (letter dislocated)
  (digit dislocated)
  (whitespace dislocated)
  (parse-grid function))

(defrule letter (or (character-ranges (#\a #\z))
                    (character-ranges (#\A #\Z))))

(defrule digit (character-ranges (#\0 #\9)))

(defrule integer (and (? #\-) (+ digit))
  (:text t)
  (:function parse-integer))

(defrule whitespace (+ (or #\Space #\Tab #\Newline)))

(defun parse-grid (data &key (container :array) (transform nil))
  (let* ((rows (count #\Newline data))
         (cols (position #\Newline data))
         (grid (ecase container
                 (:array (make-array (list rows cols)))
                 (:hash (make-hash-table :test #'equal)))))
    (flet ((import-row (row str)
             (dotimes (col cols)
               (ecase container
                 (:array (setf (aref grid row col) (funcall transform (char str col))))
                 (:hash (setf (gethash (cons row col) grid) (funcall transform (char str col))))))))
      (loop for i = 0 then (1+ i)
            for row in (split "\\n" data)
            do (import-row i row)))
    grid))
