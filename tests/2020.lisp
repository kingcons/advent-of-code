(defpackage aoc.tests.2020
  (:use :cl :try)
  (:nicknames :tests.2020)
  (:export #:test-2020))

(in-package :tests.2020)

;; NOTE: To run this test file, execute `(asdf:test-system :advent)' in your Lisp.

;;;; Day 01

(deftest day01-part1 ()
  (let ((items '(1721 979 366 299 675 1456)))
    (is (= (2020.01:find-pair items) 514579))))

(deftest day01-part2 ()
  (let ((items '(1 2 3 4 5 6 7 8 9 20 979 366 675)))
    (is (= (2020.01:find-triple items) 241861950))))

;;;; Day 02

(deftest day02-part1 ()
  (let ((items '((1 3 #\a "abcde")
                 (1 3 #\b "cdefg")
                 (2 9 #\c "ccccccccc"))))
    (is (= (2020.02:count-valid items) 2))))

(deftest day02-part2 ()
  (let ((items '((1 3 #\a "abcde")
                 (1 3 #\b "cdefg")
                 (2 9 #\c "ccccccccc"))))
    (is (= (2020.02:count-xor-valid items) 1))))

;;;; Day 03

(defparameter *map*
  '("..##......."
    "#...#...#.."
    ".#....#..#."
    "..#.#...#.#"
    ".#...##..#."
    "..#.##....."
    ".#.#.#....#"
    ".#........#"
    "#.##...#..."
    "#...##....#"
    ".#..#...#.#"))

(deftest day03-part1 ()
  (is (= (2020.03:count-trees *map*) 7)))

(deftest day03-part2 ()
  (is (= (2020.03:count-collisions *map*) 336)))

;;;; Day 04

(defparameter *passports*
  '((("ecl" "gry") ("pid" "860033327") ("eyr" "2020") ("hcl" "#fffffd")
     ("byr" "1937") ("iyr" "2017") ("cid" "147") ("hgt" "183cm"))
    (("iyr" "2013") ("ecl" "amb") ("cid" "350") ("eyr" "2023") ("pid" "028048884")
     ("hcl" "#cfa07d") ("byr" "1929"))
    (("hcl" "#ae17e1") ("iyr" "2013") ("eyr" "2024") ("ecl" "brn")
     ("pid" "760753108") ("byr" "1931") ("hgt" "179cm"))
    (("hcl" "#cfa07d") ("eyr" "2025") ("pid" "166559648") ("iyr" "2011")
     ("ecl" "brn") ("hgt" "59in"))
    ))

(defparameter *more-passports*
  '((("eyr" "1972") ("cid" "100") ("hcl" "#18171d") ("ecl" "amb") ("hgt" "170")
     ("pid" "186cm") ("iyr" "2018") ("byr" "1926"))
    (("iyr" "2019") ("hcl" "#602927") ("eyr" "1967") ("hgt" "170cm") ("ecl" "grn")
     ("pid" "012533040") ("byr" "1946"))
    (("hcl" "dab227") ("iyr" "2012") ("ecl" "brn") ("hgt" "182cm")
     ("pid" "021572410") ("eyr" "2020") ("byr" "1992") ("cid" "277"))
    (("hgt" "59cm") ("ecl" "zzz") ("eyr" "2038") ("hcl" "74454a") ("iyr" "2023")
     ("pid" "3556412378") ("byr" "2007"))
    (("pid" "087499704") ("hgt" "74in") ("ecl" "grn") ("iyr" "2012")
     ("eyr" "2030") ("byr" "1980") ("hcl" "#623a2f"))
    (("eyr" "2029") ("ecl" "blu") ("cid" "129") ("byr" "1989") ("iyr" "2014")
     ("pid" "896056539") ("hcl" "#a97842") ("hgt" "165cm"))
    (("hcl" "#888785") ("hgt" "164cm") ("byr" "2001") ("iyr" "2015") ("cid" "88")
     ("pid" "545766238") ("ecl" "hzl") ("eyr" "2022"))
    (("iyr" "2010") ("hgt" "158cm") ("hcl" "#b6652a") ("ecl" "blu") ("byr" "1944")
     ("eyr" "2021") ("pid" "093154719"))))

(deftest day04-part1 ()
  (is (= (2020.04:count-non-polar-ids *passports*) 2)))

;;;; Day 05

(defparameter *boarding-passes*
  '("FBFBBFFRLR"
    "BFFFBBFRRR"
    "FFFBBBFRRR"
    "BBFFBBFRLL"))

(deftest day05-part1 ()
  (is (= (2020.05:highest-seat-id *boarding-passes*) 820)))

;;;; Day 06

(defparameter *groups*
  '(((#\a #\b #\c))
    ((#\a) (#\b) (#\c))
    ((#\a #\b) (#\a #\c))
    ((#\a) (#\a) (#\a) (#\a))
    ((#\b))))

(deftest day06-part1 ()
  (is (= (2020.06:count-groups *groups* #'union) 11)))

(deftest day06-part2 ()
  (is (= (2020.06:count-groups *groups* #'intersection) 6)))

;;;; Day 07

(defparameter *rules*
  '((:light-red (:bright-white . 1) (:muted-yellow . 2))
    (:dark-orange (:bright-white . 3) (:muted-yellow . 4))
    (:bright-white (:shiny-gold . 1))
    (:muted-yellow (:shiny-gold . 2) (:faded-blue . 9))
    (:shiny-gold (:dark-olive . 3) (:vibrant-plum . 2))
    (:dark-olive (:faded-blue . 3) (:dotted-black . 4))
    (:vibrant-plum (:faded-blue . 5) (:dotted-black . 6))
    (:faded-blue)
    (:dotted-black)))

(defparameter *fancy-rules*
  '((:shiny-gold (:dark-red . 2))
    (:dark-red (:dark-orange . 2))
    (:dark-orange (:dark-yellow . 2))
    (:dark-yellow (:dark-green . 2))
    (:dark-green (:dark-blue . 2))
    (:dark-blue (:dark-violet . 2))
    (:dark-violet)))

(deftest day07-part1 ()
  (let ((graph (2020.07::build-graph *rules*)))
    (is (= (2020.07:count-containers :shiny-gold graph) 4))))

(deftest day07-part2 ()
  (let ((graph (2020.07::build-graph *fancy-rules*)))
    (is (= (2020.07:count-contents :shiny-gold graph) 126))))

;;;; Day 08

(defparameter *code*
  #((:nop 0)
    (:acc 1)
    (:jmp 4)
    (:acc 3)
    (:jmp -3)
    (:acc -99)
    (:acc 1)
    (:jmp -4)
    (:acc 6)))

;; (deftest day08-part1
;;   (is (= (screamer:one-value (2020.08:exit-value (2020.08:final-acc-value *code*))) 5)))

;; (deftest day08-part2
;;   (is (= (2020.08:final-acc-value *code*) 8)))

;;;; Day 09

(defparameter *xmas*
  #(35
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
    576))

(deftest day09-part1 ()
  (let ((2020.09::*width* 5))
    (is (= (2020.09:find-invalid-number *xmas*) 127))))

(deftest day09-part2 ()
  (let ((2020.09::*width* 5))
    (is (= (2020.09:find-invalid-slice *xmas*) 62))))

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
  (day09-part1)
  (day09-part2))

#+nil
(test-2020)
