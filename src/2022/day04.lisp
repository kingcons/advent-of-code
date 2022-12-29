(mgl-pax:define-package :aoc.2022.04
  (:nicknames :2022.04)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :cl-ppcre #:register-groups-bind))

(in-package :2022.04)

(defsummary (:title "Camp Cleanup")
  "Day 4 gives us a list of ranges of numbers, two to a line.
We are asked to count up the lines where one range is entirely
contained by another. We just need a way to determine if one range
is contained by, or a subset of, another range."

  "**Parsing**

Parsing is more involved today so we break out the cl-ppcre library, specifically
[REGISTER-GROUPS-BIND](https://edicl.github.io/cl-ppcre/#register-groups-bind).
PARSE-ASSIGNMENT leans on it to turn each line into a list of 4 integers. In the
future, I'd like to rewrite this using PEG parsing from
[esrap](https://scymtym.github.io/esrap/) instead of regexes."
  (parsing-source
   (include (:start (parse-assignment function) :end (subset? function))
            :header-nl "```common-lisp" :footer-nl "```"))

  "**Part 1**

Common Lisp's COUNT-IF function is ideal for this puzzle, accepting
a callback to run on each element in the given sequence. All we need
to do is write a SUBSET? check that can look at the start and end of
the two ranges and determine if one is entirely contained in the other."
  (part-1-source
   (include (:start (subset? function) :end (overlap? function))
            :header-nl "```common-lisp" :footer-nl "```"))

  "**Part 2**

Part 2 complicates things slightly by asking to detect overlapping
ranges rather than subsets, but this is still simple to test for.
The OVERLAP? function handles this and acts as our callback for part 2."
  (part-2-source
   (include (:start (overlap? function) :end (part-2 function))
            :header-nl "```common-lisp" :footer-nl "```")))

(defun parse-assignment (assignment)
  (register-groups-bind ((#'parse-integer a1 a2 b1 b2))
      ("(\\d+)-(\\d+),(\\d+)-(\\d+)" assignment)
    (list a1 a2 b1 b2)))

(defun build-data (&optional input)
  (read-day-input #'parse-assignment :input input))

(defun subset? (assignment)
  (destructuring-bind (a1 a2 b1 b2) assignment
    (or (<= b1 a1 a2 b2)
        (<= a1 b1 b2 a2))))

(defun part-1 (&optional (data (build-data)))
  (count-if #'subset? data))

(defun overlap? (assignment)
  (destructuring-bind (a1 a2 b1 b2) assignment
    (or (<= a1 b1 b2 a2)
        (<= b1 a1 a2 b2)
        (<= a1 b1 a2 b2)
        (<= b1 a1 b2 a2))))

(defun part-2 (&optional (data (build-data)))
  (count-if #'overlap? data))
