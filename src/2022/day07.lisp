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

(defsummary (:title "No Space Left on Device")
  "**Parsing**"
  (parsing-source
   (include (:start (*first-rule* variable) :end (change-directory function))
            :header-nl "```common-lisp" :footer-nl "```"))

  "**Part 1**"
  (part-1-source
   (include (:start (change-directory function) :end (smallest-matching function))
            :header-nl "```common-lisp" :footer-nl "```"))

  "**Part 2**"
  (part-2-source
   (include (:start (smallest-matching function) :end (part-2 function))
            :header-nl "```common-lisp" :footer-nl "```")))

(defvar *first-rule*
  (defrule filename (+ (or letter #\. #\/))
    (:text t)))

(defrule filespec (and integer " " filename)
  (:function first))

(defrule dir-entry (and "dir " filename)
  (:constant nil))

(defrule command (and "$ " (or "ls" "cd") (? (and " " filename)))
  (:function third)
  (:function lastcar)) ;; Ignore anything but filename

(defrule entry (or command filespec dir-entry))

(defun build-data (&optional input)
  (read-day-input (partial #'parse 'entry) :compact t :input input))

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

(defun part-1 (&optional (data (build-data)))
  (total-size-matching (op (< _ 100000)) data))

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

(defun part-2 (&optional (data (build-data)))
  (smallest-matching (op (> _ (needed-space data))) data))
