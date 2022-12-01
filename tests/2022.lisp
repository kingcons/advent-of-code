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

(deftest test-2022 ()
  (day01-part1)
  (day01-part2))

#+nil
(test-2022)
