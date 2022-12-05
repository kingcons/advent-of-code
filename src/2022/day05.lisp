(mgl-pax:define-package :aoc.2022.05
  (:nicknames :2022.05)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :serapeum #:~>>
                          #:dict
                          #:op))

(in-package :2022.05)

(defsummary (:title "Supply Stacks")
  "**Part 1** - "

  "**Part 2** - ")

(defparameter *stacks*
  (dict 1 '(p z m t r c n)
        2 '(z b s t n d)
        3 '(g t c f r q h m)
        4 '(z r g)
        5 '(h r n z)
        6 '(d l z p w s h f)
        7 '(m g c r z d w)
        8 '(q z w h l f j s)
        9 '(n w p q s)))

(defun move-crates (count origin destination)
  (dotimes (i count)
    (let ((item (pop (gethash origin *stacks*))))
      (push item (gethash destination *stacks*)))))

(defun interpret (moves stacks &key step-fn)
  (loop for (count origin destination) in moves
        do (funcall step-fn count origin destination))
  (loop for i from 1 upto 9
        collecting (first (gethash i stacks))))

(defun part-1 ()
  (let ((data (read-day-input #'parse-stacks :separator "\\n\\n" :whole t)))
    (interpret data *stacks* :step-fn #'move-crates)))

(defun move-crates-contiguous (count origin destination)
  (let ((to-move (loop for i below count
                       collecting (pop (gethash origin *stacks*)))))
    (loop for crate in (reverse to-move)
          do (push crate (gethash destination *stacks*)))))

(defun part-2 ()
  (let ((data (read-day-input #'parse-stacks :separator "\\n\\n" :whole t)))
    (interpret data *stacks* :step-fn #'move-crates-contiguous)))
