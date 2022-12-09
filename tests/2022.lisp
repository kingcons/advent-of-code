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

(deftest day01-part1 ()
  (is (= 24000 (2022.01::part-1 *day01-input*))))

(deftest day01-part2 ()
  (is (= 45000 (2022.01::part-2 *day01-input*))))

;;;; Day 02

(defvar *day02-input*
  "A Y
B X
C Z")

(deftest day02-part1 ()
  (is (= 15 (2022.02::part-1 *day02-input*))))

(deftest day02-part2 ()
  (is (= 12 (2022.02::part-2 *day02-input*))))

;;;; Day 03

(defvar *day03-input*
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(deftest day03-part1 ()
  (is (= 157 (2022.03::part-1 *day03-input*))))

(deftest day03-part2 ()
  (is (= 70 (2022.03::part-2 *day03-input*))))

;;;; Day 04

(defvar *day04-input*
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(deftest day04-part1 ()
  (is (= 2 (2022.04::part-1 *day04-input*))))

(deftest day04-part2 ()
  (is (= 4 (2022.04::part-2 *day04-input*))))

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
  (is (string= "CMZ" (2022.05::part-1 *day05-input*))))

(deftest day05-part2 ()
  (is (string= "MCD" (2022.05::part-2 *day05-input*))))

;;;; Day 06

(defvar *day06-input*
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(deftest day06-part1 ()
  (is (= 7 (2022.06::part-1 *day06-input*))))

(deftest day06-part2 ()
  (is (= 19 (2022.06::part-2 *day06-input*))))

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
  (is (= 95437 (2022.07::part-1 *day07-input*))))

(deftest day07-part2 ()
  (is (= 24933642 (2022.07::part-2 *day07-input*))))

;;;; Day 08

(defvar *day08-input*
  "30373
25512
65332
33549
35390")

(deftest day08-part1 ()
  (is (= 21 (2022.08::part-1 *day08-input*))))

(deftest day08-part2 ()
  (is (= 8 (2022.08::part-2 *day08-input*))))

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
  (day07-part2)
  (day08-part1)
  (day08-part2))

#+nil
(test-2022)
