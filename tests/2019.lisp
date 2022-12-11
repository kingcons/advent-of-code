(defpackage aoc.tests.2019
  (:use :cl :try)
  (:nicknames :tests.2019)
  (:export #:test-2019))

(in-package :tests.2019)

;;;; Day 01

(defvar *day01-input*
  "12
14
1969
100756")

(deftest day01-part1 ()
  "Computing fuel demands based on mass"
  (let ((data (2019.01::build-data *day01-input*)))
    (is (= (2019.01::part-1 data) 34241))))

(deftest day01-part2 ()
  "Compute the fixed point of fuel requirements"
  (let ((data (2019.01::build-data *day01-input*)))
    (is (= (2019.01::part-2 data) 51316))))

(deftest test-2019 ()
  (day01-part1)
  (day01-part2))

#+nil
(test-2019)
