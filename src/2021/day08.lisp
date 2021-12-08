(mgl-pax:define-package :aoc.2021.08
  (:nicknames :2021.08)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :uiop :strcat))

(in-package :2021.08)

(defsection @2021.08 (:title "Seven Segment Search")
  (@part-1 section)
  (@part-2 section))

(defsection @part-1 (:title ""))

(defstruct entry
  (signals #() :type (simple-array string))
  (outputs #() :type (simple-array string)))

(defun parse-segment (input)
  (let ((strings (coerce (cl-ppcre:split "\\s+" input) 'vector)))
    (make-entry :signals (subseq strings 0 10)
                :outputs (subseq strings 11))))

(defun count-unique-digits (entries &aux (unique-lengths '(2 3 4 7)))
  (flet ((is-unique-digit? (string)
           (member (length string) unique-lengths)))
    (loop for entry in entries
          sum (count-if #'is-unique-digit? (entry-outputs entry)))))

(defun part-1 ()
  (let ((data (read-day-input #'parse-segment)))
    (summarize  (count-unique-digits data))))

(defsection @part-2 (:title ""))

(defun compute-mapping (signals)
  (flet ((all-and-one-new (new seen)
           (and (every (lambda (x) (find x new)) seen)
                (= 1 (length (string-trim seen new))))))
    (let* ((mapping (make-hash-table :test #'equal))
           (one-segments (find 2 signals :key #'length))
           (seven-segments (find 3 signals :key #'length))
           (four-segments (find 4 signals :key #'length))
           (seen (remove-duplicates (strcat one-segments seven-segments four-segments)))
           (nine-segments (find-if (lambda (x) (all-and-one-new x seen)) signals))
           (zero-segments nil)
           (eight-segments nil)
           top middle bottom tl tr bl br)
      (setf top (string-trim one-segments seven-segments)
            bottom (string-trim seen nine-segments)
            seen (strcat seen bottom)
            eight-segments (find 7 signals :key #'length)
            bl (string-trim seen eight-segments)
            seen eight-segments
            zero-segments (let ((charbag (strcat top bottom bl one-segments)))
                            (find-if (lambda (x) (all-and-one-new x charbag)) signals))
            tl (string-trim (strcat top bottom bl one-segments) zero-segments)
            middle (string-trim zero-segments four-segments))
      ;; TODO - confirmed top middle bottom tl bl
      (let* ((almost-two (strcat top middle bottom bl))
             (two-segments (find-if (lambda (x) (all-and-one-new x almost-two)) signals)))
        (setf tr (string-trim almost-two two-segments)
              br (string-trim two-segments one-segments)
              tl (string-trim (strcat top middle bottom tr br) nine-segments)))
      (flet ((insert (value charbag)
               (let ((sorted (sort charbag #'char<)))
                 (setf (gethash sorted mapping) (coerce value 'character)))))
        (insert "0" zero-segments)
        (insert "1" one-segments)
        (insert "2" (strcat top middle bottom bl tr))
        (insert "3" (strcat top middle bottom tr br))
        (insert "4" four-segments)
        (insert "5" (strcat top middle bottom tl br))
        (insert "6" (strcat top middle bottom tl bl br))
        (insert "7" seven-segments)
        (insert "8" (strcat top middle bottom tl tr bl br))
        (insert "9" nine-segments))
      mapping)))

(defun compute-output (entry)
  (with-slots (signals outputs) entry
    (let ((mapping (compute-mapping signals)))
      (flet ((translate (x)
               (let ((sorted (sort x #'char<)))
                 (gethash sorted mapping))))
        (parse-integer (map 'string #'translate outputs))))))

(defun sum-outputs (entries)
  (reduce #'+ (mapcar #'compute-output entries)))

(defun part-2 ()
  (let ((data (read-day-input #'parse-segment)))
    (summarize (sum-outputs data))))
