(mgl-pax:define-package :aoc.2022.01
  (:nicknames :2022.01)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :cl-ppcre :split)
  (:import-from :serapeum :~>>))

(in-package :2022.01)

(defsummary (:title "Calorie Counting")
  "As usual, the first day is a straightforward warmup problem.
We are given a list of lists of integers and asked to find the
sublist that has the largest sum. Each integer is on a separate
line and the sublists are separated by a blank line."

  "**Parsing**

Parsing is made much easier thanks to my READ-DAY-INPUT macro.
I can use its `:separator` option to group the input into sublists
and then map over the sublists with PARSE-INVENTORY.

Inside PARSE-INVENTORY I make use of Serapeum's handy threading macro
[~>>](https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#-needle-rest-holes)
to construct a pipeline for parsing and summing the elements in the sublist."
  (parsing-source
   (include (:start (parse-inventory function) :end (part-1 function))
            :header-nl "```common-lisp" :footer-nl "```"))

  "**Part 1**

With so much work handled in the parsing step, we can simply use
Lisp's MAX on the parsed input to arrive at the answer for part 1."
  (part-1-source
   (include (:start (part-1 function) :end (total-snacks function))
            :header-nl "```common-lisp" :footer-nl "```"))
  
  "**Part 2**

Part 2 asks us to figure out the total of the 3 largest sublists.
Easy enough. We'll lean on the `~>>` macro again and simply sort
the parsed input, then sum the first 3 items."
  (part-2-source
   (include (:start (total-snacks function) :end (part-2 function))
            :header-nl "```common-lisp" :footer-nl "```")))

(defun parse-inventory (inventory)
  (~>> (split "\\n" inventory)
       (mapcar #'parse-integer)
       (reduce #'+)))

(defun build-data (&optional input)
  (read-day-input #'parse-inventory :separator "\\n\\n" :input input))

(defun part-1 (&optional (data (build-data)))
  (apply 'max data))

(defun total-snacks (snacks)
  (~>> (sort snacks #'>)
       (subseq _ 0 3)
       (reduce #'+)))

(defun part-2 (&optional (data (build-data)))
  (total-snacks data))
