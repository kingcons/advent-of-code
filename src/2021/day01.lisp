(mgl-pax:define-package :aoc.2021.01
  (:nicknames :2021.01)
  (:use :cl :aoc.util :mgl-pax))

(in-package :2021.01)

(defsummary (:title "Sonar Sweep")
  "**Part 1** - Into the Depths

For part 1, we'll just be checking how often a depth reading
is increasing from the previous measurement. Pretty straightforward."
  (count-depths function)
  "**Part 2** - De-noising the Depths

Part 2 extends our initial solution by averaging the depth readings
in a sliding window three at a time. I'm still using a straightforward
loop but the partitioning of the list is ugly. Two options for improvement are:

1. Solve the problem in two passes, first generating sums then counting increases.
2. Factor out the size of the window, either via callback or some other means.

Interesting notes after more experiments. I've tried a number of different approaches
to this problem, some that are flexible enough to accommodate both parts of the puzzle
and some that are specialized to the 3-element window of the second part.

Looking at the generic versions that can account for any input size we have 3 speed tiers:

1. COUNT-DISPLACED-DEPTHS which is depressingly slow due to use of displaced arrays.
   Neat feature that they are, it seems displaced arrays are just plain slow. ~5x slower.

2. COUNT-WINDOWED-DEPTHS which uses REDUCE and LOOP to compute the result. ~2.5x-3 slower.

3. COUNT-INCREASING-SUMS-LOOP which is a direct translation of death's solution using DO.
   I often enjoy reviewing death's solutions as he tends to have different ideas than I do
   and I think he has good taste and knowledge of the language. I don't much like DO though.

It's worth pointing out that my COUNT-WINDOWED-DEPTHS is flexible enough to accept a list or array
as an input. ...Though the performance degrades substantially from ~2.5x slower to 170x slower lol.
This is due to repeated traversals of the input list by reduce.

Notably, it was much easier to write a version using lists instead of arrays that conses _and_
uses 2 passes but because it is specialized on a 3-element version, it is just as fast as the
fastest generic version. Granted, I haven't tried it on larger inputs. COUNT-AVERAGE-DEPTHS is
clean and simple but I was curious if I could eliminate the consing and go faster. After some
experimentation, I wound up with COUNT-SHIFTING-DEPTHS which is 4-5x faster than
COUNT-INCREASING-SUMS-LOOP! Very interesting to see how much of a difference
specializing makes in this case."
  (count-average-depths function)
  (count-windowed-depths function))

(defun build-data (&optional input)
  (read-day-input #'parse-integer :input input))

(defun count-depths (sonar-readings)
  (loop for (previous depth) on sonar-readings
        while depth
        counting (> depth previous)))

(defun part-1 (&optional (data (build-data)))
  (count-depths data))

(defun count-average-depths (sonar-readings)
  (let ((sums (loop for (first second third) on sonar-readings
                    while third collect (+ first second third))))
    (loop for (previous current) on sums
          while current count (> current previous))))

(deftype ub32 ()
  '(unsigned-byte 32))

(defun count-shifting-depths (readings)
  (declare (optimize speed))
  (loop with (x y z) of-type ub32
        with count of-type fixnum = 0 and previous of-type fixnum = 0
        for depth in readings
        do (progn
             (shiftf x y z depth)
             (when (> (+ x y z) previous)
               (incf count))
             (setf previous (+ x y z)))
        finally (return (- count 3))))

(defun count-windowed-depths (readings window-size)
  (loop for i from 0 upto (- (length readings) window-size)
        with prev = (reduce #'+ readings :start i :end (+ i window-size))
        for sum = (reduce #'+ readings :start i :end (+ i window-size))
        count (> sum prev)
        do (setf prev sum)))

(defun count-increasing-sums-loop (input k)
  (loop for i below (- (length input) k)
        with prev = (reduce #'+ input :end k)
        for sum = (+ (- prev (aref input i))
                     (aref input (+ i k)))
        count (> sum prev)
        do (setf prev sum)))

(defun count-displaced-depths (readings window-size)
  (let ((window (make-array window-size :displaced-to readings)))
    (flet ((advance (offset)
             (adjust-array window window-size :displaced-to readings :displaced-index-offset offset)))
      (loop with previous = 0 and count = 0 and offset = 0
            while (< offset (- (length readings) window-size))
            do (let ((sum (reduce #'+ window)))
                 (declare (fixnum sum previous count))
                 (when (> sum previous)
                   (incf count))
                 (setf previous sum)
                 (advance (incf offset)))
            finally (return count)))))

(defun part-2 (&optional (data (build-data)))
  (count-shifting-depths data))
