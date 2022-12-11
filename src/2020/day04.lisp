(mgl-pax:define-package :aoc.2020.04
  (:nicknames :2020.04)
  (:use :cl :aoc.util :mgl-pax)
  (:import-from :cl-ppcre #:regex-replace-all #:split))

(in-package :2020.04)

(defsummary (:title "Passport Processing")
  "**Part 1** - Check required fields"
  (count-non-polar-ids function)
  "**Part 2** - Validate fields"
  (count-valid-passports function))

(defparameter *required-fields*
  '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

(defun build-data (&optional input)
  (read-day-input #'parse-identification :separator "\\n\\n" :input input))

(defun has-required-fields? (id)
  (let ((keys (mapcar #'first id)))
    (subsetp *required-fields* keys :test #'string=)))

(defun count-non-polar-ids (ids)
  (loop for id in ids counting (has-required-fields? id)))

(defun part-1 (&optional (data (build-data)))
  (count-non-polar-ids data))

(defun valid-byr? (id)
  (let ((value (cadr (assoc "byr" id :test #'string=))))
    (and value
         (= (length value) 4)
         (<= 1920 (parse-integer value) 2002))))

(defun valid-iyr? (id)
  (let ((value (cadr (assoc "iyr" id :test #'string=))))
    (and value
         (= (length value) 4)
         (<= 2010 (parse-integer value) 2020))))

(defun valid-eyr? (id)
  (let ((value (cadr (assoc "eyr" id :test #'string=))))
    (and value
         (= (length value) 4)
         (<= 2020 (parse-integer value) 2030))))

(defun valid-hgt? (id)
  (let ((value (cadr (assoc "hgt" id :test #'string=))))
    (and value
         (cl-ppcre:register-groups-bind ((#'parse-integer number) unit)
             ("(\\d+)(\\w+)" value)
           (if (string= unit "cm")
               (<= 150 number 193)
               (and (string= unit "in")
                    (<= 59 number 76)))))))

(defun valid-hcl? (id)
  (let ((value (cadr (assoc "hcl" id :test #'string=))))
    (and value
         (cl-ppcre:scan "#[0-9a-f]{6}" value))))

(defun valid-ecl? (id)
  (let ((value (cadr (assoc "ecl" id :test #'string=))))
    (and value
         (member value '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string=))))

(defun valid-pid? (id)
  (let ((value (cadr (assoc "pid" id :test #'string=))))
    (and value
         (cl-ppcre:scan "[\\d]{9}" value))))

(defun parse-identification (data)
  (let ((fields (split "\\s+" (regex-replace-all "\\n" data " "))))
    (mapcar (lambda (field) (split ":" field)) fields)))

(let ((validator (alexandria:conjoin #'valid-byr? #'valid-iyr? #'valid-eyr?
                                     #'valid-hgt? #'valid-hcl? #'valid-ecl?
                                     #'valid-pid?)))
  (defun valid-passport? (id)
    (funcall validator id)))

(defun count-valid-passports (ids)
  (loop for id in ids counting (valid-passport? id)))

(defun part-2 (&optional (data (build-data)))
  (count-valid-passports data))
