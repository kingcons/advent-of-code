(mgl-pax:define-package :aoc.2021.04
  (:nicknames :2021.04)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :cl-ppcre #:split))

(in-package :2021.04)

(defsummary (:title "Giant Squid")
  "**Part 1** - Bingo Subsystem"
  (play-bingo function)
  "**Part 2** - Let the Squid Win"
  (play-bingo-final function))

(defvar *wins*
  '((0 1 2 3 4)
    (5 6 7 8 9)
    (10 11 12 13 14)
    (15 16 17 18 19)
    (20 21 22 23 24)
    (0 5 10 15 20)
    (1 6 11 16 21)
    (2 7 12 17 22)
    (3 8 13 18 23)
    (4 9 14 19 24)
    (0 6 12 18 24)
    (4 8 12 16 20)))

(defun build-data (&optional input)
  (read-day-input #'identity :separator "\\n\\n" :input input))

(defun parse-nums-by (regex string)
   (map 'vector #'parse-integer (remove "" (cl-ppcre:split regex string) :test #'string=)))

(defun find-winner (chosen boards)
  (dolist (board boards)
    (dolist (win *wins*)
      (let ((numbers (mapcar (lambda (x) (aref board x)) win)))
        (when (every (lambda (x) (member x chosen)) numbers)
          (return-from find-winner board))))))

(defun score-for (chosen board)
  (* (first chosen)
     (reduce #'+ (remove-if (lambda (x) (member x chosen)) board))))

(defun play-bingo (order boards)
  (loop with chosen = '()
        for number across order
        for winner = (find-winner chosen boards)
        until winner
        do (push number chosen)
        finally (return (score-for chosen winner))))

(defun part-1 (&optional (data (build-data)))
  (let ((order (parse-nums-by "," (first data)))
        (boards (mapcar (lambda (board) (parse-nums-by "\\s+" board)) (rest data))))
    (play-bingo order boards)))

(defun filter-boards (order boards)
  (loop with chosen = '()
        for number across order
        do (loop for winner = (find-winner chosen boards)
                 while winner
                 do (setf boards (remove winner boards :test #'equalp)))
        until (= 1 (length boards))
        do (push number chosen)
        finally (return (list chosen order (1- (length chosen)) boards))))

(defun play-bingo-final (order boards)
  (destructuring-bind (chosen order index boards) (filter-boards order boards)
    (loop for number = (aref order (incf index))
          for winner = (find-winner chosen boards)
          until winner
          do (push number chosen)
          finally (return (score-for chosen winner)))))

(defun part-2 (&optional (data (build-data)))
  (let ((order (parse-nums-by "," (first data)))
        (boards (mapcar (lambda (board) (parse-nums-by "\\s+" board)) (rest data))))
    (play-bingo-final order boards)))
