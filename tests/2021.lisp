(defpackage aoc.tests.2021
  (:use :cl :try)
  (:nicknames :tests.2021)
  (:export #:test-2021))

(in-package :tests.2021)

;;;; Day 01

(deftest day01-part1 ()
  (let ((items '(199 200 208 210 200 207 240 269 260 263)))
    (is (= (2021.01::count-depths items) 7))))

(deftest day01-part2 ()
  (let ((items '(199 200 208 210 200 207 240 269 260 263)))
    (is (= (2021.01::count-shifting-depths items) 5))))

;;;; Day 02

;;;; Day 03

(deftest day03-part1 ()
  (let ((items '("00100" "11110" "10110" "10111" "10101" "01111"
                 "00111" "11100" "10000" "11001" "00010" "01010")))
    (is (= (2021.03::get-power-consumption items) 198))))

(deftest day03-part2 ()
  (let ((items '("00100" "11110" "10110" "10111" "10101" "01111"
                 "00111" "11100" "10000" "11001" "00010" "01010")))
    (is (= (2021.03::get-life-support items) 230))))

(deftest test-2021 ()
  (tests.2021::day01-part1)
  (tests.2021::day01-part2)
  (tests.2021::day03-part1)
  (tests.2021::day03-part2))

#+nil
(test-2021)
