(mgl-pax:define-package :aoc.2019.01
  (:nicknames :2019.01)
  (:use :cl :aoc.util :mgl-pax))

(in-package :2019.01)

(defsection @2019.01 (:title "Day 1 - Fuel Requirements")
  (@fuel-requirements section)
  (fuel-requirements-1 function)
  (fuel-requirements-2 function)
  (fuel-requirements-3 function)
  (@total-fuel section)
  (total-fuel-needed-1 function)
  (total-fuel-needed-2 function)
  (total-fuel-needed-3 function))

(defsection @fuel-requirements (:title "Part 1")
  "Part 1 is just a simple summation problem.
We need to compute the total fuel requirements based on a list of
masses provided. To make things a little interesting I wrote three
variations, one with REDUCE, one with DOLIST, and one with LOOP.
The different versions were almost equivalent, taking ~10k CPU cycles
and executing in a handful of microseconds. I added a type declaration
to the LOOP version for giggles and saw a ~50% speedup which is reflected
in the disassembly being a tighter 124 bytes compared to 276 bytes for
the DOLIST version and 371 bytes for the functional version.

```common-lisp
(let ((data (read-day-input #'parse-integer)))
  (time (fuel-requirements-3 data)))

;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000002 seconds of total run time (0.000002 user, 0.000000 system)
;;   100.00% CPU
;;   4,426 processor cycles
;;   0 bytes consed
```")

(defun fuel-requirements-1 (masses)
  "Compute the fuel requirements for the list of MASSES.

```cl-transcript
(let ((masses '(12 14 1969 100756)))
  (2019.01:fuel-requirements-1 masses))
=> 34241
```"
  (flet ((fuel-for (x) (- (floor x 3) 2)))
    (reduce #'+ (mapcar #'fuel-for masses))))

(defun fuel-requirements-2 (masses)
  (let ((sum 0))
    (dolist (mass masses)
      (incf sum (- (floor mass 3) 2)))
    sum))

(defun fuel-requirements-3 (masses)
  (loop for mass of-type fixnum in masses
        summing (- (floor mass 3) 2)))

(defsection @total-fuel (:title "Part 2")
  "To extend the problem, we'll compute a fixed point for the fuel.
Similar to the first part, I wrote a few different variations on this problem.
The first was a classic tail recursive approach, the second used nested LOOPs,
and the final was a monolithic LOOP. I was a bit surprised to see the
monolithic LOOP consistently outperform the other two approaches.
The macroexpansion for the nested loops is a lot bulkier and I suspect at
the end of the day SBCL's optimizer just can't eliminate all the cruft.

```common-lisp
(let ((data (read-day-input #'parse-integer)))
  (time (total-fuel-needed-3 data)))

;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000008 seconds of total run time (0.000007 user, 0.000001 system)
;;   100.00% CPU
;;   16,976 processor cycles
;;   0 bytes consed
```")

(defun total-fuel-needed-1 (masses)
  "Compute the total fuel needed for MASSES.

```cl-transcript
(let ((masses '(12 14 1969 100756)))
  (2019.01:total-fuel-needed-1 masses))
=> 51316
```"
  (labels ((fixed-point (x &optional (acc 0))
             (let ((fuel (- (floor x 3) 2)))
               (if (plusp fuel)
                   (fixed-point fuel (+ acc fuel))
                   acc))))
    (reduce #'+ (mapcar #'fixed-point masses))))

(defun total-fuel-needed-2 (masses)
  (flet ((fuel-for (initial-mass)
           (- (floor initial-mass 3) 2)))
    (loop for mass in masses
          summing (loop for fuel = (fuel-for mass) then (fuel-for fuel)
                        while (plusp fuel)
                        sum fuel))))

(defun total-fuel-needed-3 (masses)
  (flet ((fuel-for (initial-mass)
           (- (floor initial-mass 3) 2)))
    (loop with total = 0
          for mass in masses
          for fuel = (fuel-for mass)
          while (plusp fuel)
          do (setf total (+ total fuel)
                   fuel (fuel-for fuel))
          finally (return total))))
