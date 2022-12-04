(defpackage aoc.tests.2022
  (:use :cl :try :aoc.util)
  (:nicknames :tests.2022)
  (:export #:test-2022))

(in-package :tests.2022)

;;;; Day 01

(defvar *day01-input*
  "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(defun day01-input ()
  (read-day-input #'2022.01:parse-inventory
                  :separator "\\n\\n"
                  :input *day01-input*))

(deftest day01-part1 ()
  (is (= (apply 'max (day01-input))
         24000)))

(deftest day01-part2 ()
  (is (= (2022.01:total-snacks (day01-input))
         45000)))

;;;; Day 02

(defvar *day02-input*
  "A Y
B X
C Z")

(deftest day02-part1 ()
  (let ((data (read-day-input #'2022.02::parse-move :input *day02-input*)))
    (is (= (2022.02:total-score data) 15))))

(deftest day02-part2 ()
  (let ((data (read-day-input #'2022.02::parse-result :input *day02-input*)))
    (is (= (2022.02:total-score data) 12))))

;;;; Day 03

(defvar *day03-input*
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(deftest day03-part1 ()
  (let ((data (read-day-input #'2022.03:locate-duplicate :input *day03-input*)))
    (is (= (2022.03::total-priority data) 157))))

(deftest day03-part2 ()
  (let ((data (read-day-input #'2022.03:locate-badge :input *day03-input* :batches-of 3)))
    (is (= (2022.03::total-priority data) 70))))

;;;; Day 04

(defvar *day04-input*
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(deftest day04-part1 ()
  (let ((data (read-day-input #'2022.04::parse-assignment :input *day04-input*)))
    (is (= (count-if #'2022.04::subset? data) 2))))

(deftest day04-part2 ()
  (let ((data (read-day-input #'2022.04::parse-assignment :input *day04-input*)))
    (is (= (count-if #'2022.04::overlap? data) 4))))

;;;; Summary

(deftest test-2022 ()
  (day01-part1)
  (day01-part2)
  (day02-part1)
  (day02-part2)
  (day03-part1)
  (day03-part2)
  (day04-part1)
  (day04-part2))

#+nil
(test-2022)
