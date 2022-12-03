(mgl-pax:define-package :aoc.2022.02
  (:nicknames :2022.02)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :alexandria #:lastcar
                            #:make-keyword)
  (:import-from :serapeum #:op))

(in-package :2022.02)

(defsection @2022.02 (:title "Rock Paper Scissors")
  "Requirements: [Day 02](https://adventofcode.com/2022/day/02)"

  "**Part 1** - Pick my move"
  (total-score function)
  "**Part 2** - Pick my objective")

(defvar *games*
  '((:a :x :draw 4)
    (:a :y :win  8)
    (:a :z :lose 3)
    (:b :x :lose 1)
    (:b :y :draw 5)
    (:b :z :win  9)
    (:c :x :win  7)
    (:c :y :lose 2)
    (:c :z :draw 6))
  "A list of all possible 1-round games of Rock, Paper, Scissors.
In the format: (opponent-move player-move result score)")

(defun parse-move (input)
  (list (make-keyword (char input 0))
        (make-keyword (char input 2))))

(defun play (input)
  (flet ((match? (game)
           (every (op (member _ game)) input)))
    (declare (dynamic-extent #'match?))
    (lastcar (find-if #'match? *games*))))

(defun total-score (games)
  (reduce #'+ (mapcar #'play games)))

(defun part-1 ()
  (let ((games (read-day-input #'parse-move)))
    (summarize (total-score games))))

(defun parse-result (input)
  (let* ((outcome (make-keyword (char input 2)))
         (outcome-map '(:x :lose :y :draw :z :win)))
    (list (make-keyword (char input 0))
          (getf outcome-map outcome))))

(defun part-2 ()
  (let ((games (read-day-input #'parse-result)))
    (summarize (total-score games))))
