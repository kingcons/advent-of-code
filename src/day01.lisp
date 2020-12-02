(defpackage :advent2020.day-01
  (:nicknames :day-01)
  (:use :cl)
  (:import-from :alexandria #:read-file-into-string #:when-let)
  (:import-from :split-sequence #:split-sequence)
  (:import-from :arrows #:-> #:->>)
  (:export #:find-pair #:find-triple))

(in-package :day-01)

(defun load-data (file)
  (->> file
    (asdf:system-relative-pathname :advent2020)
    (read-file-into-string)
    (split-sequence #\Newline)
    (butlast)
    (mapcar #'parse-integer)))

(defun find-pair (items)
  (dolist (item-1 items)
    (dolist (item-2 items)
      (when (= (+ item-1 item-2) 2020)
        (return-from find-pair (* item-1 item-2))))))

(defun find-triple (items &aux (hash (make-hash-table)))
  (dolist (item items)
    (setf (gethash item hash) item))
  (dolist (item-1 items)
    (dolist (item-2 items)
      (when-let (item-3 (gethash (- 2020 item-1 item-2) hash))
        (return-from find-triple (* item-1 item-2 item-3))))))

(defun part01 (file)
  (->> file (load-data) (find-pair)))

(defun part02 (file)
  (->> file (load-data) (find-triple)))
