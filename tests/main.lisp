(defpackage advent2020/tests/main
  (:use :cl
        :advent2020
        :rove))
(in-package :advent2020/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :advent2020)' in your Lisp.

(deftest day01-part1
  (let ((items '(1721 979 366 299 675 1456)))
    (is (day-01:fix-expense-report items) 514579)))
