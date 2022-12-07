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

;;;; Day 05

(defvar *day05-input*
  "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(deftest day05-part1 ()
  (let ((data (read-day-input #'2022.05::parse-stacks :input *day05-input*
                                                      :separator "\\n\\n" :whole t)))
    (is (string= "CMZ" (2022.05::interpret data :step-fn #'2022.05::move-crates)))))

(deftest day05-part2 ()
  (let ((data (read-day-input #'2022.05::parse-stacks :input *day05-input*
                                                      :separator "\\n\\n" :whole t)))
    (is (string= "MCD" (2022.05::interpret data :step-fn #'2022.05::move-crates-contiguous)))))

;;;; Day 06

(defvar *day06-input*
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(deftest day06-part1 ()
  (is (= (2022.06::parse-signal *day06-input* 4) 7)))

(deftest day06-part2 ()
  (is (= (2022.06::parse-signal *day06-input* 14) 19)))

;;;; Day 07

(defvar *day07-input*
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(deftest day07-part1 ()
  (let* ((data (read-day-input #'2022.07::parse-terminal :compact t :input *day07-input*)))
    (is (= (2022.07::total-size-matching (serapeum:op (< _ 100000)) data) 95437))))

(deftest day07-part2 ()
  (let* ((data (read-day-input #'2022.07::parse-terminal :compact t :input *day07-input*)))
    (is (= (2022.07::smallest-matching (serapeum:op (> _  (2022.07::needed-space data))) data) 24933642))))

;;;; Summary

(deftest test-2022 ()
  (day01-part1)
  (day01-part2)
  (day02-part1)
  (day02-part2)
  (day03-part1)
  (day03-part2)
  (day04-part1)
  (day04-part2)
  (day05-part1)
  (day05-part2)
  (day06-part1)
  (day06-part2)
  (day07-part1)
  (day07-part2))

#+nil
(test-2022)
