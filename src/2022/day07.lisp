(mgl-pax:define-package :aoc.2022.07
  (:nicknames :2022.07)
  (:use :cl :aoc.util :mgl-pax :esrap)
  (:import-from :aoc.parsers
                #:letter
                #:integer)
  (:import-from :alexandria
                #:emptyp
                #:hash-table-values
                #:lastcar)
  (:import-from :serapeum
                #:~>>
                #:op
                #:partial))

(in-package :2022.07)

(defsummary (:title "")
  "**Part 1** - "

  "**Part 2** - ")

(defrule filename (+ (or letter #\. #\/))
  (:text t))

(defrule filespec (and integer " " filename)
  (:function first))

(defrule dir-entry (and "dir " filename)
  (:constant nil))

(defrule command (and "$ " (or "ls" "cd") (? (and " " filename)))
  (:function third)
  (:function lastcar)) ;; Ignore anything but filename

(defrule entry (or command filespec dir-entry))

(defun parse-terminal (input)
  (parse 'entry input))

(defun change-directory (entry)
  (declare (special *current-directory*))
  (if (string= ".." entry)
      (pop *current-directory*)
      (push entry *current-directory*)))

(defun count-file (entry table)
  (declare (special *current-directory*))
  (loop with directory = (copy-list *current-directory*)
        until (emptyp directory)
        do (incf (gethash directory table 0) entry)
           (pop directory)))

(defun record-entry (entry table)
  (if (stringp entry)
      (change-directory entry)
      (count-file entry table)))

(defun compute-directory-sizes (data)
  (let ((table (make-hash-table :test #'equal))
        (*current-directory* nil))
    (declare (special *current-directory*))
    (dolist (entry data)
      (record-entry entry table))
    table))

(defun total-size-matching (match-fn data)
  (~>> (compute-directory-sizes data)
       (hash-table-values)
       (remove-if-not match-fn)
       (reduce #'+)))

(defun part-1 ()
  (let ((data (read-day-input #'parse-terminal :compact t)))
    (total-size-matching (op (< _ 100000)) data)))

(defun free-space (data)
  (- 70000000 (gethash '("/") data)))

(defun smallest-matching (match-fn data)
  (~>> (compute-directory-sizes data)
       (hash-table-values)
       (remove-if-not match-fn)
       (apply 'min)))

(defun needed-space (data)
  (~>> (compute-directory-sizes data)
       (gethash '("/"))
       (- 70000000)
       (- 30000000)))

(defun part-2 ()
  (let* ((data (read-day-input #'parse-terminal :compact t)))
    (smallest-matching (op (> _ (needed-space data))) data)))
