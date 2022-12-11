(defpackage aoc.tests.2020
  (:use :cl :try)
  (:nicknames :tests.2020)
  (:export #:test-2020))

(in-package :tests.2020)

;;;; Day 01

(defvar *day01-input*
  "1721
979
366
299
675
1456")

(deftest day01-part1 ()
  (let ((data (2020.01::build-data *day01-input*)))
    (is (= (2020.01::part-1 data) 514579))))

(deftest day01-part2 ()
  (let ((data (2020.01::build-data *day01-input*)))
    (is (= (2020.01::part-2 data) 241861950))))

;;;; Day 02

(defvar *day02-input*
  "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc")

(deftest day02-part1 ()
  (let ((data (2020.02::build-data *day02-input*)))
    (is (= (2020.02::part-1 data) 2))))

(deftest day02-part2 ()
  (let ((data (2020.02::build-data *day02-input*)))
    (is (= (2020.02::part-2 data) 1))))

;;;; Day 03

(defvar *day03-input*
  "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(deftest day03-part1 ()
  (let ((data (2020.03::build-data *day03-input*)))
    (is (= (2020.03::part-1 data) 7))))

(deftest day03-part2 ()
  (let ((data (2020.03::build-data *day03-input*)))
    (is (= (2020.03::part-2 data) 336))))

;;;; Day 04

(defvar *day04-input*
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(deftest day04-part1 ()
  (let ((data (2020.04::build-data *day04-input*)))
    (is (= (2020.04::part-1 data) 2))))

;;;; Day 05

(defvar *day05-input*
  "FBFBBFFRLR")

(deftest day05-part1 ()
  (let ((data (2020.05::build-data *day05-input*)))
    (is (= (2020.05::part-1 data) 357))))

;;;; Day 06

(defvar *day06-input*
  "abc

a
b
c

ab
ac

a
a
a
a

b")

(deftest day06-part1 ()
  (let ((data (2020.06::build-data *day06-input*)))
    (is (= (2020.06::part-1 data) 11))))

(deftest day06-part2 ()
  (let ((data (2020.06::build-data *day06-input*)))
    (is (= (2020.06::part-2 data) 6))))

;;;; Day 07

(defvar *day07-input*
  "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(defvar *day07-input2*
  "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.")

(deftest day07-part1 ()
  (let ((data (2020.07::build-data *day07-input*)))
    (is (= (2020.07::part-1 data) 4))))

(deftest day07-part2 ()
  (let ((data (2020.07::build-data *day07-input2*)))
    (is (= (2020.07::part-2 data) 126))))

;;;; Day 08

(defvar *day08-input*
  "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(deftest day08-part1 ()
  (let ((data (2020.08::build-data *day08-input*)))
    (is (= (2020.08::part-1 data) 5))))

(deftest day08-part2 ()
  (let ((data (2020.08::build-data *day08-input*)))
    (is (= (2020.08::part-2 data) 8))))

;;;; Day 09

(defvar *day09-input*
  "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")

(deftest day09-part1 ()
  (let ((data (2020.09::build-data *day09-input*))
        (2020.09::*width* 5))
    (is (= (2020.09::part-1 data) 127))))

(deftest day09-part2 ()
  (let ((data (2020.09::build-data *day09-input*))
        (2020.09::*width* 5))
    (is (= (2020.09::part-2 data) 62))))

(deftest test-2020 ()
  (day01-part1)
  (day01-part2)
  (day02-part1)
  (day02-part2)
  (day03-part1)
  (day03-part2)
  (day04-part1)
  (day05-part1)
  (day06-part1)
  (day06-part2)
  (day07-part1)
  (day07-part2)
  (day08-part1)
  (day08-part2)
  (day09-part1)
  (day09-part2))

#+nil
(test-2020)
