(mgl-pax:define-package :aoc.2022.02
  (:nicknames :2022.02)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :alexandria #:assoc-value
                            #:make-keyword))

(in-package :2022.02)

(defsection @2022.02 (:title "Rock Paper Scissors")
  "Requirements: [Day 02](https://adventofcode.com/2022/day/02)"

  "**Part 1** - "

  "**Part 2** - ")

(defvar *strategy*
  '((:a . :y)
    (:b . :x)
    (:c . :z))
  "A mapping from opponent inputs (:a for rock, :b for paper, :c for scissors)
to shapes to play (:x for rock, :y for paper, :z for scissors).")

(defvar *outcome*
  '(((:a :x) . :draw)
    ((:a :y) . :win)
    ((:a :z) . :lose)
    ((:b :x) . :lose)
    ((:b :y) . :draw)
    ((:b :z) . :win)
    ((:c :x) . :win)
    ((:c :y) . :lose)
    ((:c :z) . :draw))
  "A mapping from game states to outcome scores.")

(defvar *shape-score*
  '((:x . 1)
    (:y . 2)
    (:z . 3))
  "A mapping from shapes to scores.")

(defvar *outcome-score*
  '((:lose . 0)
    (:draw . 3)
    (:win . 6))
  "A mapping from outcomes to scores.")

(defun parse-game (game)
  (list (make-keyword (char game 0))
        (make-keyword (char game 2))))

(defun score-game (game)
  (let* ((shape-score (assoc-value *shape-score* (second game)))
         (outcome (assoc-value *outcome* game :test #'equal))
         (outcome-score (assoc-value *outcome-score* outcome)))
    (+ shape-score
       outcome-score)))

(defun part-1 ()
  (let ((data (read-day-input #'parse-game)))
    (summarize (reduce #'+ (mapcar #'score-game data)))))

(defvar *secret-strategy*
  '(((:a :x) . :z)
    ((:a :y) . :x)
    ((:a :z) . :y)
    ((:b :x) . :x)
    ((:b :y) . :y)
    ((:b :z) . :z)
    ((:c :x) . :y)
    ((:c :y) . :z)
    ((:c :z) . :x))
  "A mapping from the game state to what to actually throw.")

(defun secret-strategy (input)
  (let* ((game (parse-game input))
         (shape (assoc-value *secret-strategy* game :test #'equal)))
    (list (first game) shape)))

(defun part-2 ()
  (let ((data (read-day-input #'secret-strategy)))
    (summarize (reduce #'+ (mapcar #'score-game data)))))
