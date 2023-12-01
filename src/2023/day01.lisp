(mgl-pax:define-package :aoc.2023.01
  (:nicknames :2023.01)
  (:use :cl :mgl-pax :aoc.util)
  (:import-from :alexandria #:when-let)
  (:import-from :serapeum
                :fmt
                :op))

(in-package :2023.01)

(defsummary (:title "Trebuchet?!")
  "**Parsing**"

  "**Part 1**"

  "**Part 2**")

(defun build-data (&optional input)
  (read-day-input #'identity :input input))

(defun calibration-value (string &key (parser #'identity))
  (let* ((input (funcall parser string))
         (first (find-if #'digit-char-p input))
         (last (find-if #'digit-char-p input :from-end t)))
    (parse-integer (fmt "~d~d" first last))))

(defun part-1 (&optional (data (build-data)))
  (loop for item in data sum (calibration-value item)))

(defun fix-numeral (target start end match-start match-end &rest args)
  (declare (ignore start end args))
  (let ((numbers '("one" "two" "three" "four" "five"
                   "six" "seven" "eight" "nine"))
        (match (subseq target match-start match-end)))
    (fmt "~d~a" (1+ (position match numbers :test #'string=))
         (subseq match (1- (length match))))))

(defun replace-digits (string)
  (let ((digits "one|two|three|four|five|six|seven|eight|nine"))
    (loop do (setf string (cl-ppcre:regex-replace digits string #'fix-numeral))
          while (cl-ppcre:scan digits string)
          finally (return string))))

(defun part-2 (&optional (data (build-data)))
  (loop for item in data
        sum (calibration-value item :parser #'replace-digits)))
