(mgl-pax:define-package :aoc.2021.08
  (:nicknames :2021.08)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :uiop :strcat))

(in-package :2021.08)

(defsummary (:title "Seven Segment Search")
  "**Part 1** - Count the Easy Ones"
  (count-unique-digits function)
  "**Part 2** - Decode the Outputs"
  (compute-output function))

(defstruct entry
  (signals #() :type (simple-array string))
  (outputs #() :type (simple-array string)))

(defun parse-segment (input)
  (let ((strings (coerce (cl-ppcre:split "\\s+" input) 'vector)))
    (make-entry :signals (subseq strings 0 10)
                :outputs (subseq strings 11))))

(defun build-data (&optional input)
  (read-day-input #'parse-segment :input input))

(defun count-unique-digits (entries &aux (unique-lengths '(2 3 4 7)))
  (flet ((is-unique-digit? (string)
           (member (length string) unique-lengths)))
    (loop for entry in entries
          sum (count-if #'is-unique-digit? (entry-outputs entry)))))

(defun part-1 (&optional (data (build-data)))
  (count-unique-digits data))

(defun compute-mapping (signals)
  (let* ((mapping (make-hash-table :test #'equal))
         (one-segments (find 2 signals :key #'length))
         (four-segments (find 4 signals :key #'length))
         (diff (remove-if (lambda (x) (find x one-segments)) four-segments)))
    (flet ((insert (value charbag)
             (setf (gethash (sort charbag #'char<) mapping) value))
           (match? (s length contains)
             (and (= length (length s))
                  (every (lambda (x) (find x s)) contains))))
      (insert #\1 one-segments)
      (insert #\4 four-segments)
      (insert #\7 (find 3 signals :key #'length))
      (insert #\8 (find 7 signals :key #'length))
      (loop for signal across signals
            do (cond ((match? signal 5 one-segments)
                      (insert #\3 signal))
                     ((match? signal 5 diff)
                      (insert #\5 signal))
                     ((match? signal 5 "")
                      (insert #\2 signal))
                     ((match? signal 6 four-segments)
                      (insert #\9 signal))
                     ((match? signal 6 diff)
                      (insert #\6 signal))
                     ((match? signal 6 "")
                      (insert #\0 signal)))))
    mapping))

(defun compute-output (entry)
  (with-slots (signals outputs) entry
    (let ((mapping (compute-mapping signals)))
      (flet ((decode (x) (gethash (sort x #'char<) mapping)))
        (parse-integer (map 'string #'decode outputs))))))

(defun sum-outputs (entries)
  (reduce #'+ (mapcar #'compute-output entries)))

(defun part-2 (&optional (data (build-data)))
  (sum-outputs data))
