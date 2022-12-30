(mgl-pax:define-package :aoc.2022.13
  (:nicknames :2022.13)
  (:use :cl :mgl-pax :aoc.util)
  (:import-from :cl-ppcre #:split)
  (:import-from :serapeum
                #:~>>
                #:compose
                #:string-replace-all))

(in-package :2022.13)

(defsummary (:title "Distress Signal")
  "**Parsing**"
  (parsing-source
   (include (:start (sanitize-packet function) :end (compare generic-function))
            :header-nl "```common-lisp" :footer-nl "```"))

  "**Part 1**"
  (part-1-source
   (include (:start (compare generic-function) :end (part-2 function))
            :header-nl "```common-lisp" :footer-nl "```"))

  "**Part 2**")

(defun sanitize-packet (packet)
  (let ((replacements '(("[" "(")
                        ("]" ")")
                        ("," " "))))
    (reduce (lambda (x y) (string-replace-all (first y) x (second y)))
            replacements :initial-value packet)))

(defun parse-packets (pair)
  (let ((read-packet (compose #'read-from-string #'sanitize-packet)))
    (~>> (split "\\n" pair)
         (mapcar read-packet))))

(defun build-data (&optional input)
  (read-day-input #'parse-packets :separator "\\n\\n" :input input))

(defgeneric compare (left right)
  (:documentation "Compare two values according to the rules of 2022.13"))

(defmethod compare ((left integer) (right integer))
  (cond ((< left right) t)
        ((> left right) nil)
        (t :equal)))

(defmethod compare ((left list) (right list))
  (dotimes (i (max (length left) (length right)))
    (when (null left) (return-from compare t))
    (when (null right) (return-from compare nil))
    (let ((result (compare (pop left) (pop right))))
      (unless (eql result :equal)
        (return-from compare result))))
  :equal)

(defmethod compare ((left list) (right integer))
  (compare left (list right)))

(defmethod compare ((left integer) (right list))
  (compare (list left) right))

(defun part-1 (&optional (data (build-data)))
  (loop for i = 0 then (1+ i)
        for (left right) in data
        when (compare left right)
          sum (1+ i)))

(defun part-2 (&optional (data (build-data)))
  (flet ((index-of (packet list) (1+ (position packet list :test #'equalp))))
    (let ((sorted (~>> (append '((((2)) ((6)))) data)
                       (reduce #'append)
                       (sort _ #'compare))))
      (* (index-of '((2)) sorted) (index-of '((6)) sorted)))))
