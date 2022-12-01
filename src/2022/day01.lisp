(mgl-pax:define-package :aoc.2022.01
  (:nicknames :2022.01)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :cl-ppcre :split)
  (:import-from :serapeum :~>>))

(in-package :2022.01)

(defsection @2022.01 (:title "Calorie Counting")
  "Requirements: [Day 01](https://adventofcode.com/2022/day/1)

As usual, the first day is a straightforward warmup problem.
A lot of the actual work is simply parsing the supplied data.
I have a few tools that make this task easier:

* [`~>>`](https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#-needle-rest-holes)
 is a handy threading macro supplied by the wonderful Serapeum library
* [READ-DAY-INPUT][aoc.util:read-day-input] is a helper macro I wrote that loads the input file for the current day,
 splits the string based on a separator and maps over the results with a supplied parsing function

Part 1 asks us to determine who has the most snacks among the elves.
Part 2 asks us to figure out the total snacks among the top 3 snack-holders.

Part 1 is easy enough to solve. We simply apply MAX after parsing the input.
Part 2 requires a little additional work to sort and sum the top 3 elves which is handled by TOTAL-SNACKS."

  (@part-1 section)
  (@part-2 section))

(defsection @part-1 (:title "Who got snacks?")
  (parse-inventory function)
  (source (include (:start (parse-inventory function) :end (part-1 function))
                   :header-nl "```common-lisp" :footer-nl "```")))

(defun parse-inventory (inventory)
  "Given a string, INVENTORY, convert each line to an integer and sum them."
  (~>> (split "\\n" inventory)
       (mapcar #'parse-integer)
       (reduce #'+)))

(defun part-1 ()
  (let ((snacks (read-day-input #'parse-inventory :separator "\\n\\n")))
    (apply 'max snacks)))

(defsection @part-2 (:title "Backup Snack Strategy")
  (total-snacks function)
  (source (include (:start (total-snacks function) :end (part-2 function))
                   :header-nl "```common-lisp" :footer-nl "```")))

(defun total-snacks (snacks)
  "Sort the supplied list of integers, SNACKS, in descending order,
   then sum the 3 largest items."
  (~>> (sort snacks #'>)
       (subseq _ 0 3)
       (reduce #'+)))

(defun part-2 ()
  (let ((snacks (read-day-input #'parse-inventory :separator "\\n\\n")))
    (total-snacks snacks)))
