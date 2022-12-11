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
  (let ((data (2022.01::build-data *day01-input*)))
    (is (= 24000 (2022.01::part-1 data)))))

(deftest day01-part2 ()
  (let ((data (2022.01::build-data *day01-input*)))
    (is (= 45000 (2022.01::part-2 data)))))

;;;; Day 02

(defvar *day02-input*
  "A Y
B X
C Z")

(deftest day02-part1 ()
  (let ((data (2022.02::build-data *day02-input*)))
    (is (= 15 (2022.02::part-1 data)))))

(deftest day02-part2 ()
  (let ((data (2022.02::build-data *day02-input*)))
    (is (= 12 (2022.02::part-2 data)))))

;;;; Day 03

(defvar *day03-input*
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(deftest day03-part1 ()
  (let ((data (2022.03::build-data *day03-input*)))
    (is (= 157 (2022.03::part-1 data)))))

(deftest day03-part2 ()
  (let ((data (2022.03::build-data *day03-input*)))
    (is (= 70 (2022.03::part-2 data)))))

;;;; Day 04

(defvar *day04-input*
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(deftest day04-part1 ()
  (let ((data (2022.04::build-data *day04-input*)))
    (is (= 2 (2022.04::part-1 data)))))

(deftest day04-part2 ()
  (let ((data (2022.04::build-data *day04-input*)))
    (is (= 4 (2022.04::part-2 data)))))

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
  (let ((data (2022.05::build-data *day05-input*)))
    (is (string= "CMZ" (2022.05::part-1 data)))))

(deftest day05-part2 ()
  (let ((data (2022.05::build-data *day05-input*)))
    (is (string= "MCD" (2022.05::part-2 data)))))

;;;; Day 06

(defvar *day06-input*
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(deftest day06-part1 ()
  (let ((data (2022.06::build-data *day06-input*)))
    (is (= 7 (2022.06::part-1 data)))))

(deftest day06-part2 ()
  (let ((data (2022.06::build-data *day06-input*)))
    (is (= 19 (2022.06::part-2 data)))))

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
  (let ((data (2022.07::build-data *day07-input*)))
    (is (= 95437 (2022.07::part-1 data)))))

(deftest day07-part2 ()
  (let ((data (2022.07::build-data *day07-input*)))
    (is (= 24933642 (2022.07::part-2 data)))))

;;;; Day 08

(defvar *day08-input*
  "30373
25512
65332
33549
35390")

(deftest day08-part1 ()
  (let ((data (2022.08::build-data *day08-input*)))
    (is (= 21 (2022.08::part-1 data)))))

(deftest day08-part2 ()
  (let ((data (2022.08::build-data *day08-input*)))
    (is (= 8 (2022.08::part-2 data)))))

;;;; Day 09

(defvar *day09-input*
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(defvar *day09-input-2*
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(deftest day09-part1 ()
  (let ((data (2022.09::build-data *day09-input*)))
    (is (= (2022.09::part-1 data) 13))))

(deftest day09-part2 ()
  (let ((data (2022.09::build-data *day09-input-2*)))
    (is (= (2022.09::part-2 data) 36))))

(deftest day09-evil-bug ()
  (let ((before '((1 -18) (2 -18) (3 -17) (3 -16) (4 -16)
                  (4 -15) (4 -14) (4 -13) (4 -12) (4 -11)))
        (after '((1 -20) (1 -19) (2 -18) (2 -17) (3 -17)
                 (3 -16) (3 -15) (3 -14) (3 -13) (3 -12))))
    (is (equalp (2022.09::update-rope '("D" 2) before (make-hash-table)) after)))
  ;; Fundamental issue is we can be off by two on both axis which was impossible before.
  (let ((head '(1 -19))
        (tail '(2 -18)))
    (is (equalp (2022.09::move-tail head tail) '(2 -18))))
  (let ((head '(1 -20))
        (tail '(2 -18)))
    (is (equalp (2022.09::move-tail head tail) '(1 -19)))))

;;;; Day 10

(defvar *day10-input*
  "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

(deftest day10-part1 ()
  (let ((data (2022.10::build-data *day10-input*)))
    (is (= (2022.10::part-1 data) 13140))))

(defvar *day10-part2*
  "##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....")

(deftest day10-part2 ()
  (let ((data (2022.10::build-data *day10-input*)))
    (is (string= (2022.10::part-2 data) *day10-part2*))))

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
  (day08-part2)
  (day09-part1)
  (day09-part2)
  (day09-evil-bug)
  (day10-part1)
  (day10-part2))

#+nil
(test-2022)
