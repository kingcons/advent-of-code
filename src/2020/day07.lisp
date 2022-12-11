(mgl-pax:define-package :aoc.2020.07
  (:nicknames :2020.07)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :alexandria #:make-keyword)
  (:import-from :graph #:digraph #:add-edge))

(in-package :2020.07)

(defsummary (:title "Handy Haversacks")
  "**Part 1** - How many bags can contain a shiny gold bag?"
  (count-containers function)
  "**Part 2** - How many bags does a shiny gold bag hold?"
  (count-contents function))

(defun build-data (&optional input)
  (read-day-input #'parse-rule :input input))

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
        finally (return (length (remove-duplicates options)))))

(defun part-1 (&optional (data (build-data)))
  (count-containers :shiny-gold (build-graph data)))

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

(defun part-2 (&optional (data (build-data)))
  (count-contents :shiny-gold (build-graph data)))
