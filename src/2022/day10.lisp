(mgl-pax:define-package :aoc.2022.10
  (:nicknames :2022.10)
  (:use :cl :mgl-pax :aoc.util :aoc.parsers :esrap)
  (:import-from :alexandria
                #:flatten
                #:make-keyword)
  (:import-from :serapeum
                #:~>>
                #:batches
                #:concat
                #:partial
                #:string-join))

(in-package :2022.10)

(defsummary (:title "Cathode-Ray Tube" :show-answer nil)
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

(defun run-program (data &key collect-fn)
  (let ((cpu (make-cpu)))
    (loop for (opcode arg) = (pop data) while opcode
          collect (funcall collect-fn cpu opcode)
          do (execute cpu opcode arg))))

(defun new-cycles (cpu opcode)
  (+ (cpu-cycles cpu) (getf *cycle-times* opcode)))

(defun sampled-cycle? (cpu opcode next-sample)
  (<= (cpu-cycles cpu) next-sample (new-cycles cpu opcode)))

(defun make-sample-fn ()
  (let ((timings '(20 60 100 140 180 220)))
    (lambda (cpu opcode)
      (when (sampled-cycle? cpu opcode (or (first timings) 0))
        (* (pop timings) (cpu-x-reg cpu))))))

(defun signal-strength (output)
  (reduce #'+ (remove nil output)))

(defun part-1 (&optional (data (build-data)))
  (signal-strength (run-program data :collect-fn (make-sample-fn))))

(defmethod render ((cpu cpu) opcode)
  (with-slots (cycles x-reg) cpu
    (with-output-to-string (out)
      (dotimes (i (getf *cycle-times* opcode))
        (let* ((crt-pixel (mod (+ cycles i) 40))
               (active-pixel? (<= (1- x-reg) crt-pixel (1+ x-reg))))
          (format out (if active-pixel? "#" ".")))))))

(defun buffer-display (output)
  (~>> (reduce #'concat output)
       (batches _ 40)
       (string-join _ #\Newline)))

(defun part-2 (&optional (data (build-data)))
  (buffer-display (run-program data :collect-fn #'render)))
