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

;;;; Day 02

;;;; Day 03

(deftest day03-part1
  (let ((items '("00100" "11110" "10110" "10111" "10101" "01111"
                 "00111" "11100" "10000" "11001" "00010" "01010")))
    (ok (2021.03::get-power-consumption items) 198)))

(deftest day03-part2
  (let ((items '("00100" "11110" "10110" "10111" "10101" "01111"
                 "00111" "11100" "10000" "11001" "00010" "01010")))
    (ok (2021.03::get-life-support items) 230)))

;; Day 04

(defvar *test-data*)

(deftest day04-part1
  (ok (2021.04::foo items) ))

(run-suite :tests.2021)
