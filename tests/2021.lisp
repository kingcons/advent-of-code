(defpackage aoc.tests.2021
  (:use :cl :rove)
  (:nicknames :tests.2021))

(in-package :tests.2021)

;;;; Day 01

(deftest day01-part1
  (let ((items '(199 200 208 210 200 207 240 269 260 263)))
    (ok (2021.01::count-depths items) 7)))

(deftest day01-part2
  (let ((items '(199 200 208 210 200 207 240 269 260 263)))
    (ok (2021.01::count-depths-sliding items) 5)))
