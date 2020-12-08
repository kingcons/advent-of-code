(defpackage :advent2020.day-08
  (:nicknames :day-08)
  (:use :cl)
  (:import-from :advent2020.util #:read-day-input)
  (:import-from :alexandria #:make-keyword)
  (:import-from :screamer #:make-variable)
  (:export #:final-acc-value))

(in-package :day-08)

(defvar *mutate* nil)
(defvar *mutations* (make-variable))
(defvar *visited* '())

(defun parse-instruction (instruction)
  (destructuring-bind (opcode arg) (cl-ppcre:split " " instruction)
    (list (make-keyword (string-upcase opcode)) (parse-integer arg))))

(defstruct cpu
  (pc 0 :type fixnum)
  (acc 0 :type fixnum))

(defstruct exit
  (cause nil :type keyword)
  (value nil :type fixnum))

(defun done? (pc destination)
  (or (member pc *visited*)
      (= pc destination)))

(screamer::defun step-cpu (cpu opcode arg)
  (when (member (cpu-pc cpu) *visited*)
    (screamer:fail))
  (screamer:local
    (setf *visited* (cons (cpu-pc cpu) *visited*))
    (let ((opcode
            (cond ((and *mutate* (not (eql opcode :acc)))
                   (screamer:either :jmp :nop))
                  (t opcode))))
      (ecase opcode
        (:nop (incf (cpu-pc cpu)))
        (:acc (setf (cpu-acc cpu) (+ (cpu-acc cpu) arg)
                    (cpu-pc cpu) (1+ (cpu-pc cpu))))
        (:jmp (setf (cpu-pc cpu) (+ (cpu-pc cpu) arg)))))))

(screamer::defun final-acc-value (code)
  (screamer:assert! (screamer:=v *mutations* (screamer:an-integer-betweenv 0 1)))
  (loop with cpu = (make-cpu)
        with destination = (length code)
        for pc = (cpu-pc cpu)
        until (done? pc destination)
        for (opcode arg) = (aref code pc)
        do (step-cpu cpu opcode arg)
        finally (return (values (cpu-acc cpu) pc))))

(defun apply-patch (code address)
  (let ((patched (map 'vector #'copy-list code)))
    (symbol-macrolet ((patch-point (first (aref patched address))))
      (when (eql patch-point :acc)
        (return-from apply-patch :skip))
      (setf patch-point (if (eql patch-point :nop) :jmp :nop)))
    patched))

(defun part-1 ()
  (let ((code (coerce (read-day-input 8 #'parse-instruction) 'vector)))
    (screamer:one-value (final-acc-value code))))

(defun part-2 ()
  (loop with code = (coerce (read-day-input 8 #'parse-instruction) 'vector)
        for i = 0 then (1+ i)
        for patched = (apply-patch code i)
        unless (eql patched :skip)
          do (multiple-value-bind (acc pc)
                 (screamer:one-value (final-acc-value patched))
               (when (= pc (length code))
                 (return-from part-2 acc)))))

;; (defun part-2 ()
;;   (let ((code (coerce (read-day-input 8 #'parse-instruction) 'vector))
;;         (*mutate* t))
;;     (screamer:all-values (exit-value (final-acc-value code)))))
