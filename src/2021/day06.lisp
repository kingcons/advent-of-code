(mgl-pax:define-package :aoc.2021.06
  (:nicknames :2021.06)
  (:use :cl :aoc.util :mgl-pax))

(in-package :aoc.2021.06)

(defsection @2021.06 (:title "Lanternfish")
  (@part-1 section)
  (@part-2 section))

(defsection @part-1 (:title "That's a big school"))

(defstruct school
  (days 0)
  (counts (make-array 9 :element-type 'fixnum)))

(defun parse-school (fishlist)
  (let ((counts (make-array 9 :element-type 'fixnum))
        (timers (mapcar #'parse-integer (cl-ppcre:split "," fishlist))))
    (dolist (timer timers)
      (incf (aref counts timer)))
    (make-school :counts counts)))

(defun tick (school)
  (with-slots (counts) school
    (alexandria:rotate counts -1)
    (setf (aref counts 6) (+ (aref counts 6) (aref counts 8)))))

(defun estimate-population (school days)
  (dotimes (i days)
    (tick school))
  (reduce #'+ (school-counts school)))

(defun part-1 ()
  (let ((data (first (read-day-input #'parse-school))))
    (summarize (estimate-population data 80))))

(defsection @part-2 (:title "Uh Oh"))

(defun part-2 ()
  (let ((data (first (read-day-input #'parse-school))))
    (summarize (estimate-population data 256))))

;; (defstruct fish
;;   (timer 8))

;; (defvar *logging* nil)

;; (defun tick (fish)
;;   (with-slots (timer) fish
;;     (if (zerop timer)
;;         (let ((new-fish (make-fish)))
;;           (setf timer 6)
;;           new-fish)
;;         (decf timer))))

;; (defun estimate-growth (days init-timer)
;;   (let ((fishes '())
;;         (new-gen '()))
;;     (push (make-fish :timer init-timer) fishes)
;;     (dotimes (i days)
;;       (dolist (fish fishes)
;;         (let ((result (tick fish)))
;;           (when (fish-p result)
;;             (push result new-gen))))
;;       (when *logging*
;;         (format t "Finished day ~D / ~D~%" i days))
;;       (setf fishes (append fishes new-gen)
;;             new-gen '()))
;;     (length fishes)))

;; (defun estimate-school (school estimates days)
;;   (let ((iterations (/ days *step-size*)))
;;     (with-slots (counts) school
;;       (flet ((update-count (timer count)
;;                (setf (gethash timer counts) (* count (gethash timer estimates)))))
;;         (dotimes (i iterations)
;;           (maphash #'update-count counts))
;;         (reduce #'+ (alexandria:hash-table-values counts))))))

;; (defvar *step-size* 16
;;   "16 is the result of (gcd 80 256), the greatest common denominator of our date targets.")

;; (defun compute-estimates (table &optional (days *step-size*))
;;   (dotimes (i 9)
;;     (when *logging*
;;       (format t ">> Computing estimate for fish of size ~D~%" i))
;;     (setf (gethash i table) (estimate-growth days i)))
;;   table)

;; (defun part-1 ()
;;   (let ((data (first (read-day-input #'parse-school)))
;;         (estimates (make-hash-table)))
;;     (compute-estimates estimates)
;;     (estimate-school data estimates 80)))
