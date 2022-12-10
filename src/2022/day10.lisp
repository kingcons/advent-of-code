(mgl-pax:define-package :aoc.2022.10
  (:nicknames :2022.10)
  (:use :cl :mgl-pax :aoc.util :aoc.parsers :esrap)
  (:import-from :alexandria #:make-keyword)
  (:import-from :serapeum
                #:op
                #:partial))

(in-package :2022.10)

(defsummary (:title "Cathode-Ray Tube")
  "**Part 1** - "

  "**Part 2** - ")

(defrule opcode (or "noop" "addx")
  (:lambda (string) (make-keyword (string-upcase string))))

(defrule args (and " " integer)
  (:function second))

(defrule instruction (and opcode (? args)))

(defun build-data (&optional input)
  (read-day-input (partial #'parse 'instruction) :input input))

(defstruct cpu
  (cycles 0 :type fixnum)
  (x-reg 1 :type fixnum))

(defvar *cycle-times* '(:noop 1 :addx 2))

(defmethod execute :after ((cpu cpu) opcode arg)
  (incf (cpu-cycles cpu) (getf *cycle-times* opcode)))

(defmethod execute ((cpu cpu) (opcode (eql :noop)) arg))

(defmethod execute ((cpu cpu) (opcode (eql :addx)) arg)
  (incf (cpu-x-reg cpu) arg))

(defun special-cycle? (cycles opcode next-sample)
  (let ((new-cycles (+ cycles (getf *cycle-times* opcode))))
    (<= cycles next-sample new-cycles)))

(defun sum-at-cycles (data desired-timings)
  (let ((cpu (make-cpu)))
    (loop with next-sample = (pop desired-timings)
          for (opcode arg) = (pop data) while next-sample
          for cycles = (cpu-cycles cpu)
          when (special-cycle? cycles opcode next-sample)
            sum (* next-sample (cpu-x-reg cpu))
            and do (setf next-sample (pop desired-timings))
          do (execute cpu opcode arg))))

(defun part-1 (&optional input)
  (sum-at-cycles (build-data input) '(20 60 100 140 180 220)))

(defun render-crt (data)
  (let ((cpu (make-cpu)))
    ))

(defun part-2 (&optional input)
  (render-crt (build-data input)))
