(mgl-pax:define-package :aoc.2022.02
  (:nicknames :2022.02)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :alexandria
                #:compose
                #:lastcar
                #:make-keyword)
  (:import-from :serapeum
                #:~>>
                #:op))

(in-package :2022.02)

(defsummary (:title "Rock Paper Scissors")
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

(defun total-score (games key-fn)
  (reduce #'+ games :key key-fn))

(defun build-data (&optional input)
  (read-day-input #'parse-move :input input))

(defun part-1 (&optional (data (build-data)))
  (total-score data #'play))

(defun choose-outcome (game)
  (let ((outcome-map '(:x :lose :y :draw :z :win)))
    (setf (second game) (getf outcome-map (second game)))
    game))

(defun part-2 (&optional (data (build-data)))
  (total-score data (compose #'play #'choose-outcome)))
