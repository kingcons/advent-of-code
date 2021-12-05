(mgl-pax:define-package :aoc.2021.03
  (:nicknames :2021.03)
  (:use :cl :aoc.util :mgl-pax))

(in-package :2021.03)

(defsection @2021.03 (:title "Binary Diagnostic")
  (@part-1 section)
  (@part-2 section))

(defsection @part-1 (:title "Check the Power Consumption"))

(defun construct-ones (nums)
  (let ((ones (make-array 12 :element-type 'fixnum)))
    (dolist (num nums)
      (loop for digit across num
            for i = 0 then (1+ i)
            when (char= digit #\1) do (incf (aref ones i))))
    ones))

(defun construct-gamma (ones threshold)
  (loop for i below 12
        collect (let ((count (aref ones i)))
                  (if (>= count threshold) #\1 #\0))))

(defun construct-epsilon (gamma)
  (loop for i in gamma
        collect (if (char= i #\0) #\1 #\0)))

(defun compute-readings (nums)
  (let* ((ones (construct-ones nums))
         (threshold (/ (length nums) 2))
         (gamma (construct-gamma ones threshold))
         (epsilon (construct-epsilon gamma)))
    (list gamma epsilon)))

(defun compute-answer (gamma epsilon)
  (* (parse-integer (coerce gamma 'string) :radix 2)
     (parse-integer (coerce epsilon 'string) :radix 2)))

(defun get-power-consumption (nums)
  (destructuring-bind (gamma epsilon) (compute-readings nums)
    (compute-answer gamma epsilon)))

(defun part-1 ()
  (let ((data (read-day-input #'identity)))
    (summarize (get-power-consumption data))))

(defsection @part-2 (:title "Verify Life Support"))

(defun count-ones (nums position)
  (flet ((bit-at (string)
           (char= #\1 (aref string position))))
    (let ((ones (count-if #'bit-at nums)))
      (if (>= ones (/ (length nums) 2)) #\1 #\0))))

(defun find-match (ratings)
  (let ((nums (copy-list ratings)))
    (loop for i = 0 then (1+ i)
          until (= (length nums) 1)
          for target = (count-ones nums i)
          do (setf nums (remove-if-not (lambda (string) (char= target (aref string i))) nums))
          finally (return (first nums)))))

(defun find-ratings (nums)
  (let* ((oxygen (find-match nums))
         (carbon (find-match nums)))
    (list oxygen carbon)))

(defun get-life-support (nums)
  (destructuring-bind (oxygen-rating carbon-rating) (find-ratings nums)
    (compute-answer oxygen-rating carbon-rating)))

(defun part-2 ()
  (let ((data (read-day-input #'identity)))
    (get-life-support data)))
