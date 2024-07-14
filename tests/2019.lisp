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

(defvar *day02-input*
  "1,9,10,3,2,3,11,0,99,30,40,50")

(deftest day02-part1 ()
  "Building an Intcode interpreter"
  (let* ((data (2019.02::build-data *day02-input*))
         (2019.02::*memory* (coerce data 'vector)))
    (is (= (2019.02::run-intcode data) 3500))))

(deftest test-2019 ()
  (day01-part1)
  (day01-part2)
  (day02-part1))

#+nil
(test-2019)
