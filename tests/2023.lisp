(defpackage aoc.tests.2023
  (:use :cl :try :aoc.util)
  (:nicknames :tests.2023)
  (:export #:test-2023))

(in-package :tests.2023)

;;;; Day 01

(defvar *day01-input1*
  "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(defvar *day01-input2*
  "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(deftest day01-part1 ()
  (let ((data (2023.01::build-data *day01-input1*)))
    (is (= 142 (2023.01::part-1 data)))))

(deftest day01-part2 ()
  (let ((data (2023.01::build-data *day01-input2*)))
    (is (= 281 (2023.01::part-2 data)))))

;;;; Day 02

(defvar *day02-input*
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(deftest day02-part1 ()
  (let ((data (2023.02::build-data *day02-input*)))
    (is (= 8 (2023.02::part-1 data)))))

(deftest day02-part2 ()
  (let ((data (2023.02::build-data *day02-input*)))
    (is (= 2286 (2023.02::part-2 data)))))

;;;; Summary

(deftest test-2023 ()
  (day01-part1)
  (day01-part2)
  (day02-part1)
  (day02-part2))

#+nil
(test-2023)
