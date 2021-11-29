(mgl-pax:define-package :aoc.2019.01
  (:nicknames :2019.01)
  (:use :cl :aoc.util :mgl-pax))

(in-package :2019.01)

(defsection @2019.01 (:title "The Tyranny of the Rocket Equation")
  (@part-1 section)
  (fuel-requirements-1 function)
  (fuel-requirements-2 function)
  (fuel-requirements-3 function)
  (@part-2 section)
  (total-fuel-needed-1 function)
  (total-fuel-needed-2 function))

(defsection @part-1 (:title "Fuel for Modules")
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

(defun fuel-for (mass)
  (declare (optimize speed)
           (type fixnum mass))
  (- (truncate mass 3) 2))

(defun fuel-requirements-1 (masses)
  "Compute the fuel requirements for the list of MASSES.

```cl-transcript
(let ((masses '(12 14 1969 100756)))
  (2019.01:fuel-requirements-1 masses))
=> 34241
```"
  (reduce #'+ (mapcar #'fuel-for masses)))

(defun fuel-requirements-2 (masses)
  (let ((sum 0))
    (dolist (mass masses)
      (incf sum (fuel-for mass)))
    sum))

(defun fuel-requirements-3 (masses)
  (loop for mass in masses
        sum (fuel-for mass)))

(defun part-1 ()
  (let ((data (read-day-input #'parse-integer)))
    (summarize (fuel-requirements-3 data))))

(defsection @part-2 (:title "Fuel for Fuel")
  "To extend the problem, we'll compute a fixed point for the fuel.
Similar to the first part, I wrote a few different variations on this problem.
The first was a classic tail recursive approach and the second used nested LOOPs,

Two interesting notes:

- SBCL seems to generate tighter assembly for `truncate` than `floor` in many cases.
- Factoring out fuel-for as a separate helper and adding type and optimize declarations there
  keeps the rest of the code clean and gets us the speed benefits of typing in LOOP, etc.

```common-lisp
(let ((data (read-day-input #'parse-integer)))
  (time (total-fuel-needed-2 data)))

;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000018 seconds of total run time (0.000017 user, 0.000001 system)
;;   100.00% CPU
;;   42,426 processor cycles
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
             (let ((fuel (fuel-for x)))
               (if (plusp fuel)
                   (fixed-point fuel (+ acc fuel))
                   acc))))
    (reduce #'+ (mapcar #'fixed-point masses))))

(defun total-fuel-needed-2 (masses)
  (loop for mass in masses
        sum (loop for fuel = (fuel-for mass) then (fuel-for fuel)
                  while (plusp fuel)
                  sum fuel)))

(defun part-2 ()
  (let ((data (read-day-input #'parse-integer)))
    (summarize (total-fuel-needed-2 data))))
