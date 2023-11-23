(defpackage :advent.tests
  (:use :cl :try)
  (:export #:test-all
           #:test-ci))

(in-package :advent.tests)

(deftest test-all ()
  (tests.2019:test-2019)
  (tests.2020:test-2020)
  (tests.2021:test-2021)
  (tests.2022:test-2022))

(defun test (&key (debug nil) (print 'unexpected) (describe 'unexpected))
  (warn-on-tests-not-run ((find-package :advent.tests))
    (print (try 'test-all :debug debug :print print :describe describe))))

(defun test-ci ()
  (unless (try:passep (try:try 'test-all))
    (uiop:quit 1)))

#+nil
(test)
