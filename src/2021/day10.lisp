(mgl-pax:define-package :aoc.2021.10
  (:nicknames :2021.10)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :alexandria #:define-constant))

(in-package :2021.10)

(defsection @2021.10 (:title "Syntax Scoring")
  (@part-1 section)
  (@part-2 section))

(defsection @part-1 (:title ""))

(define-constant +error-points+
    '((#\) 3)
      (#\] 57)
      (#\} 1197)
      (#\> 25137))
  :test #'equal)

(define-constant +tokens+
    '((#\( #\))
      (#\[ #\])
      (#\{ #\})
      (#\< #\>))
  :test #'equal)

(defun score-error (line)
  (flet ((lookup (key map)
           (second (assoc key map))))
    (loop with stack = '()
          for token across line
          do (let ((previous (first stack)))
               (cond ((member token +tokens+ :key #'first)
                      (push token stack))
                     ((char= token (lookup previous +tokens+))
                      (pop stack))
                     (t
                      (return-from score-error (lookup token +error-points+))))))))

(defun score-syntax (input)
  (reduce #'+ (remove nil (mapcar #'score-error input))))

(defun part-1 ()
  (let ((data (read-day-input #'identity)))
    (summarize (score-syntax data))))

(defsection @part-2 (:title ""))

(define-constant +autocomplete-points+
    '((#\) 1)
      (#\] 2)
      (#\} 3)
      (#\> 4))
  :test #'equal)

(defun score-completion (string)
  (loop with total = 0
        for char across string
        do (let ((value (second (assoc char +autocomplete-points+))))
             (setf total (+ (* total 5) value)))
        finally (return total)))

(defun score-incomplete (line)
  (flet ((build-completion (stack)
           (loop for token = (pop stack) while token
                 collect (second (assoc token +tokens+)) into completion
                 finally (return (score-completion (coerce completion 'string))))))
    (loop with stack = '()
          for token across line
          do (let ((previous (first stack)))
               (cond ((member token +tokens+ :key #'first)
                      (push token stack))
                     ((char= token (second (assoc previous +tokens+)))
                      (pop stack))))
          finally (return (build-completion stack)))))

(defun score-corrections (input)
  (let* ((eligible (remove-if #'score-error input))
         (results (mapcar #'score-incomplete eligible)))
    (nth (floor (length results) 2) (sort results #'>))))

(defun part-2 ()
  (let ((data (read-day-input #'identity)))
    (summarize (score-corrections data))))
