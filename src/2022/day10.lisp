(mgl-pax:define-package :aoc.2022.10
  (:nicknames :2022.10)
  (:use :cl :mgl-pax :aoc.util :aoc.parsers :esrap)
  (:import-from :alexandria #:make-keyword)
  (:import-from :serapeum
                #:~>>
                #:batches
                #:concat
                #:partial
                #:string-join))

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

(defun new-cycles (cpu opcode)
  (+ (cpu-cycles cpu) (getf *cycle-times* opcode)))

(defun special-cycle? (cpu opcode next-sample)
  (<= (cpu-cycles cpu) next-sample (new-cycles cpu opcode)))

(defun sum-at-cycles (data desired-timings)
  (let ((cpu (make-cpu)))
    (loop with next-sample = (pop desired-timings)
          for (opcode arg) = (pop data) while next-sample
          when (special-cycle? cpu opcode next-sample)
            sum (* next-sample (cpu-x-reg cpu))
            and do (setf next-sample (pop desired-timings))
          do (execute cpu opcode arg))))

(defun part-1 (&optional (data (build-data)))
  (sum-at-cycles data '(20 60 100 140 180 220)))

(defmethod render ((cpu cpu) opcode arg)
  (with-slots (cycles x-reg) cpu
    (list (with-output-to-string (out)
            (dotimes (i (getf *cycle-times* opcode))
              (let* ((crt-pixel (mod (+ cycles i) 40))
                     (active-pixel? (<= (1- x-reg) crt-pixel (1+ x-reg))))
                (if active-pixel?
                    (format out "#")
                    (format out "."))))))))

(defun render-crt (data)
  (let ((cpu (make-cpu)))
    (loop for (opcode arg) = (pop data) while opcode
          for cycles = (cpu-cycles cpu)
          append (render cpu opcode arg) into output
          do (execute cpu opcode arg)
          finally (return (~>> (reduce #'concat output)
                               (batches _ 40)
                               (string-join _ #\Newline))))))

(defun part-2 (&optional (data (build-data)))
  (render-crt data))
