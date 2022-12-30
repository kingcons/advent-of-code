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
  "The second day was actually pretty interesting this year. It asks us to
interpret a series of single-round rock paper scissors games. Each line in the
input consists of two moves, one for the opponent and the player. There's a
heuristic for scoring the games and we are asked to find the total score for the
games played. Even though the ask is simple and I found an brute force solution
quickly, I went through three iterations before arriving at something I really
liked. The insight that satisfied me was that there are only 9 possible games of
single-round RPS, so we can model the scoring as a search problem."

  "**Parsing**

Parsing is simple, I just convert the characters in each line
to symbols. This simplifies equality testing as Common Lisp uses
different predicates for symbols and strings. (EQL vs EQUAL)"

  (parsing-source
   (include (:start (parse-move function) :end (*games* variable))
            :header-nl "```common-lisp" :footer-nl "```"))
  
  "**Part 1**

First we model the possible *GAMES* and their scores based on the
puzzle requirements. The PLAY function will take a list of attributes
from a game and find the unique matching game, returning its score.
With that in place, TOTAL-SCORE can be a simple reducer over the games."
  (part-1-source
   (include (:start (*games* variable) :end (choose-outcome function))
            :header-nl "```common-lisp" :footer-nl "```"))

  "**Part 2**

The curveball in Part 2 is that we should treat the player choice not as the move
to throw, but the outcome to achieve. Since we know the opponent's move in advance,
we can just map the second input to an outcome and reuse PLAY's matching behavior."
  (part-2-source
   (include (:start (choose-outcome function) :end (part-2 function))
            :header-nl "```common-lisp" :footer-nl "```")))

(defun parse-move (input)
  (list (make-keyword (char input 0))
        (make-keyword (char input 2))))

(defun build-data (&optional input)
  (read-day-input #'parse-move :input input))

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

(defun play (input)
  (flet ((match? (game)
           (every (op (member _ game)) input)))
    (declare (dynamic-extent #'match?))
    (lastcar (find-if #'match? *games*))))

(defun total-score (games key-fn)
  (reduce #'+ games :key key-fn))

(defun part-1 (&optional (data (build-data)))
  (total-score data #'play))

(defun choose-outcome (game)
  (let ((outcome-map '(:x :lose :y :draw :z :win)))
    (setf (second game) (getf outcome-map (second game)))
    (play game)))

(defun part-2 (&optional (data (build-data)))
  (total-score data #'choose-outcome))
