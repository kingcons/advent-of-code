(defpackage aoc.tests.2021
  (:use :cl :try)
  (:nicknames :tests.2021)
  (:export #:test-2021))

(in-package :tests.2021)

;;;; Day 01

(defvar *day01-input*
  "199
200
208
210
200
207
240
269
260
263")

(deftest day01-part1 ()
  (let ((data (2021.01::build-data *day01-input*)))
    (is (= (2021.01::part-1 data) 7))))

(deftest day01-part2 ()
  (let ((data (2021.01::build-data *day01-input*)))
    (is (= (2021.01::part-2 data) 5))))

;;;; Day 02

;;;; Day 03

(defvar *day03-input*
  "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(deftest day03-part1 ()
  (let ((data (2021.03::build-data *day03-input*)))
    (is (= (2021.03::part-1 data) 198))))

(deftest day03-part2 ()
  (let ((data (2021.03::build-data *day03-input*)))
    (is (= (2021.03::part-2 data) 230))))

(deftest test-2021 ()
  (day01-part1)
  (day01-part2)
  (day03-part1)
  (day03-part2))

#+nil
(test-2021)
