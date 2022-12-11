(mgl-pax:define-package :aoc.2021.03
  (:nicknames :2021.03)
  (:use :cl :aoc.util :mgl-pax))

(in-package :2021.03)

(defsummary (:title "Binary Diagnostic")
  "**Part 1** - Check the Power Consumption"
  (get-power-consumption function)
  "**Part 2** - Verify Life Support"
  (get-life-support function))

(defun build-data (&optional input)
  (read-day-input #'identity :input input))

(defun bits-to-int (bits)
  (parse-integer (coerce bits 'string) :radix 2))

(defun char-at (position)
  (lambda (string)
    (char string position)))

(defun count-ones-at (nums position)
  (flet ((is-one? (char) (char= #\1 char)))
    (count-if #'is-one? nums :key (char-at position))))

(defun gamma-rate (nums)
  (let ((target (/ (length nums) 2))
        (size (length (first nums))))
    (loop for i below size
          for count = (count-ones-at nums i)
          collect (if (> count target) #\1 #\0))))

(defun epsilon-rate (gamma)
  (loop for c in gamma collect (if (char= c #\1) #\0 #\1)))

(defun get-power-consumption (nums)
  (let* ((gamma (gamma-rate nums))
         (epsilon (epsilon-rate gamma)))
    (* (bits-to-int gamma) (bits-to-int epsilon))))

(defun part-1 (&optional (data (build-data)))
  (get-power-consumption data))

(defun correct-bit? (test ones target)
  (lambda (char)
    (char= char (if (funcall test ones target) #\1 #\0))))

(defun find-rating (nums test)
  (let ((report (copy-list nums)))
    (loop until (= 1 (length report))
          for i = 0 then (1+ i)
          for target = (/ (length report) 2)
          for ones = (count-ones-at report i)
          do (setf report (delete-if-not (correct-bit? test ones target) report :key (char-at i)))
          finally (return (first report)))))

(defun get-life-support (nums)
  (let ((oxygen (find-rating nums #'>=))
        (carbon (find-rating nums #'<)))
    (* (bits-to-int oxygen) (bits-to-int carbon))))

(defun part-2 (&optional (data (build-data)))
  (get-life-support data))
