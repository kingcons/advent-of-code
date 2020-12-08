(defpackage :advent2020.day-07
  (:nicknames :day-07)
  (:use :cl)
  (:import-from :advent2020.util #:read-day-input)
  (:import-from :alexandria #:make-keyword)
  (:import-from :graph #:digraph #:add-edge)
  (:export #:build-graph #:count-containers #:count-contents))

(in-package :day-07)

(defun parse-rule (rule)
  (flet ((build-node (item)
           (cons (make-keyword (string-upcase (subseq item 2)))
                 (parse-integer item :end 1))))
    (let* ((split (cl-ppcre:split " bags? contain (no other)?| bags?, | bags?." rule))
           (items (mapcar (lambda (x) (substitute #\- #\Space x :start 2)) split)))
      (list* (make-keyword (string-upcase (first items)))
             (mapcar #'build-node (rest items))))))

(defun build-graph (rules)
  (loop with graph = (make-instance 'digraph)
        for (container . contains) in rules
        do (loop for (color . weight) in contains
                 do (add-edge graph (list container color) weight))
        finally (return graph)))

(defun count-containers (color graph)
  (loop with to-process = (graph:precedents graph color)
        with options = (copy-list to-process)
        for node = (pop to-process)
        do (let ((precedents (graph:precedents graph node)))
             (setf options (append precedents options)
                   to-process (append precedents to-process)))
        until (null to-process)
        finally (return (remove-duplicates options))))

(defgeneric descendents (digraph node)
  (:documentation "Return all nodes succeeding NODE in an edge of DIGRAPH.")
  (:method ((digraph digraph) node)
    (let ((nodes (mapcar (lambda (e) (list (cadr e) (graph:edge-value digraph e)))
                         (graph:node-edges digraph node))))
      (remove node nodes :key #'first))))

(defun count-contents (color graph)
  (let ((descendents (descendents graph color)))
    (if (null descendents)
        0
        (loop for (node weight) in descendents
              summing (+ weight (* weight (count-contents node graph)))))))

(defun part-1 ()
  (let ((rules (read-day-input 7 #'parse-rule)))
    (count-containers :shiny-gold (build-graph rules))))

(defun part-2 ()
  (let ((rules (read-day-input 7 #'parse-rule)))
    (count-contents :shiny-gold (build-graph rules))))
