(mgl-pax:define-package :aoc.2020.07
  (:nicknames :2020.07)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :alexandria
                #:hash-table-values
                #:make-keyword))

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
  (let ((graph (make-hash-table)))
    (loop for (container . edges) in rules
          do (setf (gethash container graph) edges)
          finally (return graph))))

(defun get-containers-of (graph node)
  (loop for neighbor being the hash-keys in graph using (hash-value edges)
        for match = (find node edges :key #'first)
        when match collect (cons neighbor (cdr match))))

(defun count-containers (graph color)
  (loop with visited = (make-hash-table)
        with to-visit = (get-containers-of graph color)
        for (node . weight) = (pop to-visit)
        do (let ((neighbors (get-containers-of graph node)))
             (setf (gethash node visited) t)
             (setf to-visit (append neighbors to-visit)))
        until (null to-visit)
        finally (return (hash-table-count visited))))

(defun part-1 (&optional (data (build-data)))
  (count-containers (build-graph data) :shiny-gold))

(defun count-contents (color graph)
  (let ((contents (gethash color graph)))
    (if (null contents)
        0
        (loop for (node . weight) in contents
              summing (+ weight (* weight (count-contents node graph)))))))

(defun part-2 (&optional (data (build-data)))
  (count-contents :shiny-gold (build-graph data)))
