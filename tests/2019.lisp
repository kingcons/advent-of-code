(defpackage aoc.tests.2019
  (:use :cl :try)
  (:nicknames :tests.2019)
  (:export #:test-2019))

(in-package :tests.2019)

;;;; Day 01

(deftest day01-part1 ()
  "Computing fuel demands based on mass"
  (let ((masses '(12 14 1969 100756))
        (demands '(2 2 654 33583)))
    (is (= (2019.01:fuel-requirements-1 masses) (reduce #'+ demands)))
    (is (= (2019.01:fuel-requirements-2 masses) (reduce #'+ demands)))
    (is (= (2019.01:fuel-requirements-3 masses) (reduce #'+ demands)))))

(deftest day01-part2 ()
  "Compute the fixed point of fuel requirements"
  (let ((masses '(12 14 1969 100756))
        (demands '(2 2 966 50346)))
    (is (= (2019.01:total-fuel-needed-1 masses) (reduce #'+ demands)))
    (is (= (2019.01:total-fuel-needed-2 masses) (reduce #'+ demands)))))

(deftest test-2019 ()
  (day01-part1)
  (day01-part2))

#+nil
(test-2019)
