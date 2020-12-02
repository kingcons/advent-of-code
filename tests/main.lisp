(defpackage advent2020/tests/main
  (:use :cl
        :rove))
(in-package :advent2020/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :advent2020)' in your Lisp.

(deftest day01-part1
  (let ((items '(1721 979 366 299 675 1456)))
    (ok (day-01:find-pair items) 514579)))

(deftest day01-part2
  (let ((items '(1 2 3 4 5 6 7 8 9 20 979 366 675)))
    (ok (day-01:find-triple items) 241861950)))

(deftest day02-part1
  (let ((items '((1 3 #\a "abcde")
                 (1 3 #\b "cdefg")
                 (2 9 #\c "ccccccccc"))))
    (ok (day-02:count-valid items) 2)))

(deftest day02-part2
  (let ((items '((1 3 #\a "abcde")
                 (1 3 #\b "cdefg")
                 (2 9 #\c "ccccccccc"))))
    (ok (day-02:count-xor-valid items) 1)))
