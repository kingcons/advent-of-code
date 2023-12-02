(mgl-pax:define-package :aoc.2023.02
  (:nicknames :2023.02)
  (:use :cl :mgl-pax :aoc.util :esrap)
  (:import-from :serapeum :op
                          :~>>
                          :plist-values))

(in-package :2023.02)

(defsummary (:title "Cube Conundrum")
  "**Parsing**"

  "**Part 1**"

  "**Part 2**")

(defun make-keyword (string)
  (intern (string-upcase string) :keyword))

(defrule color (and integer " " (or "red" "green" "blue") (? ", "))
  (:lambda (result)
    (list (make-keyword (third result)) (first result))))

(defrule cubeset (and (+ color) (? "; "))
  (:function first))

(defun build-counts (game)
  (let ((counts '()))
    (dolist (set (fourth game))
      (loop for (color count) in set
            for current = (getf counts color 0)
            do (setf (getf counts color 0) (max current count))))
    (list (second game) counts)))

(defrule game (and "Game " integer ": " (+ cubeset) (? #\Newline))
  (:function build-counts))

(defun build-data (&optional input)
  (read-day-input (op (parse 'game _)) :input input))

(defun impossible? (limits counts)
  (or (> (getf counts :red) (getf limits :red))
      (> (getf counts :blue) (getf limits :blue))
      (> (getf counts :green) (getf limits :green))))

(defun part-1 (&optional (data (build-data)))
  (let ((limits '(:red 12 :green 13 :blue 14)))
    (~>> (remove-if (op (impossible? limits _)) data :key #'second)
         (reduce #'+ _ :key #'first))))

(defun part-2 (&optional (data (build-data)))
  (flet ((power (game)
           (reduce #'* (plist-values (second game)))))
    (reduce #'+ data :key #'power)))
