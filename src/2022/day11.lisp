(mgl-pax:define-package :aoc.2022.11
  (:nicknames :2022.11)
  (:use :cl :mgl-pax :aoc.util :aoc.parsers :esrap)
  (:import-from :serapeum
                #:~>>
                #:deq
                #:enq
                #:partial
                #:queue))

(in-package :2022.11)

(defsummary (:title "Monkey in the Middle")
  "**Parsing**"
  (parsing-source
   (include (:start (*first-rule* variable) :end (monkey class))
            :header-nl "```common-lisp" :footer-nl "```"))

  "**Part 1**"
  (part-1-source
   (include (:start (monkey class) :end (part-2 function))
            :header-nl "```common-lisp" :footer-nl "```"))

  "**Part 2**")

(defvar *first-rule*
  (defrule monkey-number (and "Monkey " integer ":" #\Newline)
    (:function second)))

(defrule item-list (+ (or integer ", "))
  (:lambda (list) (remove-if-not #'integerp list)))

(defrule starting-items (and spaces "Starting items: " item-list #\Newline)
  (:lambda (list) (apply 'queue (third list))))

(defrule operator (or "+" "*")
  (:function find-symbol))

(defrule inspect-op (and spaces "Operation: new = old "
                         operator " " (or integer "old") #\Newline)
  (:destructure (spaces title operator space operand newline)
    (declare (ignore spaces title space newline))
    (if (equal operand "old")
        (lambda (x) (funcall operator x x))
        (lambda (x) (funcall operator operand x)))))

(defrule test-op (and spaces "Test: divisible by " integer #\Newline)
  (:function third))

(defrule true-op (and spaces "If true: throw to monkey " integer #\Newline)
  (:function third))

(defrule false-op (and spaces "If false: throw to monkey " integer (? #\Newline))
  (:function third))

(defrule monkey (and monkey-number starting-items inspect-op
                     test-op true-op false-op (? #\Newline))
  (:destructure (num items op divisor true-fn false-fn newline)
    (declare (ignore newline))
    (make-monkey :number num :items items :operation op :divisor divisor
                 :true-recv true-fn :false-recv false-fn :inspected 0)))

(defun build-data (&optional input)
  (read-day-input (partial #'parse 'monkey) :separator "\\n\\n" :input input))

(defstruct monkey
  number items operation divisor true-recv false-recv inspected)

(defmethod pass-item (item (monkey monkey))
  (with-slots (items) monkey
    (enq item items)))

(defmethod play-round ((monkey monkey) all-monkeys worry-fn)
  (with-slots (items operation divisor true-recv false-recv inspected) monkey
    (loop for item = (deq items) while item
          do (let ((worry-level (funcall worry-fn (funcall operation item))))
               (incf inspected)
               (if (zerop (mod worry-level divisor))
                   (pass-item worry-level (nth true-recv all-monkeys))
                   (pass-item worry-level (nth false-recv all-monkeys)))))))

(defun compute-monkey-business (monkeys)
  (~>> (mapcar #'monkey-inspected monkeys)
       (sort _ #'>)
       (subseq _ 0 2)
       (apply '*)))

(defun play (monkeys rounds worry-fn &optional (debug nil))
  (dotimes (i rounds)
    (when debug
      (format t "Starting round ~d~%" i))
    (dolist (monkey monkeys)
      (play-round monkey monkeys worry-fn)))
  (compute-monkey-business monkeys))

(defun part-1 (&optional (data (build-data)))
  (play data 20 (lambda (x) (floor x 3))))

(defun part-2 (&optional (data (build-data)))
  (let ((divisor (reduce #'* (mapcar #'monkey-divisor data))))
    (play data 10000 (lambda (x) (mod x divisor)))))
