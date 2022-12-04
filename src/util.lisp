(mgl-pax:define-package :aoc.util
  (:use :cl :mgl-pax)
  (:import-from :asdf #:system-relative-pathname)
  (:import-from :alexandria #:read-file-into-string
                            #:with-unique-names)
  (:import-from :cl-ppcre #:split
                          #:regex-replace-all)
  (:import-from :serapeum #:~>>
                          #:batches
                          #:fmt))

(in-package :aoc.util)

(defsection @aoc.util (:title "Useful Utilities")
  (*aoc-session* variable)
  (build-package-name-from-pathname function)
  (build-section-from-pathname function)
  (defsummary macro)
  (read-day-input macro)
  (scaffold function)
  (summarize macro))

(defmacro extract-date-from-string (string &body body)
  "Bind YEAR and DAY to values extracted from STRING by the regex
`(\\d{4}).*(\\d{2})` and run BODY in the scope of those bindings."
  `(cl-ppcre:register-groups-bind (year day)
       ("(\\d{4}).*(\\d{2})" ,string)
     ,@body))

(defun build-package-name-from-pathname (pathname)
  "Given a PATHNAME, build a package designator appropriate for that path."
  (extract-date-from-string (namestring pathname)
    (fmt "~d.~d" year day)))

(defun build-section-from-pathname (pathname)
  "Given a PATHNAME, build a section locative appropriate for that path."
  (extract-date-from-string (namestring pathname)
    (let ((section-name (fmt "@~d.~d" year day))
          (package-name (fmt "~d.~d" year day)))
      (list (find-symbol section-name package-name)
            'section))))

(defvar *aoc-session* nil
  "A token for the user's Advent of Code session. This must be supplied
for SCAFFOLD to automatically fetch puzzle input for a given day.")

(defun make-advent-uri (year day)
  (fmt "https://adventofcode.com/~d/day/~d/input" year day))

(defun make-aoc-cookie ()
  (let ((cookie (make-instance 'drakma:cookie
                               :name "session"
                               :domain ".adventofcode.com"
                               :value *aoc-session*)))
    (make-instance 'drakma:cookie-jar :cookies (list cookie))))

(defun scaffold-input (year day)
  (let ((dat-file (day-file year day)))
    (with-open-file (out dat-file :direction :output :if-does-not-exist :create)
      (~>> (drakma:http-request (make-advent-uri year day) :cookie-jar (make-aoc-cookie))
           (format out)))))

(defun scaffold-code (year day)
  (let ((template (system-relative-pathname :advent "src/day.tmpl"))
        (lisp-file (day-file year day :extension "lisp")))
    (with-open-file (out lisp-file :direction :output :if-does-not-exist :create)
      (~>> (read-file-into-string template)
           (regex-replace-all "~4d" _ (fmt "~d" year))
           (regex-replace-all "~2d" _ (fmt "~2,'0d" day))
           (format out)))))

(defun scaffold (year day)
  "Create a new lisp file for YEAR and DAY based on the `day.tmpl` template.
An error will be thrown if a directory matching YEAR does not exist."
  (scaffold-code year day)
  (unless (null *aoc-session*)
    (scaffold-input year day)))

(defun day-file (year day &key (extension "dat"))
  (let ((filename (fmt "src/~d/day~2,'0d.~a" year day extension)))
    (system-relative-pathname :advent filename)))

(defun read-dat-file-for-package ()
  (extract-date-from-string (package-name *package*)
    (read-file-into-string (day-file year day))))

(defmacro read-day-input (item-parser &key (separator "\\n")
                          (batches-of nil) (whole nil) (input nil))
  "Load the input data for the current day based on the name of *PACKAGE*, splitting
the file based on the value of SEPARATOR and processing each string with ITEM-PARSER.
If BATCHES-OF is supplied, divide the separated data into chunks of the desired size.
If WHOLE is non-nil, after splitting pass to ITEM-PARSER directly instead of mapping.
If INPUT is supplied, use that instead of loading the DAT file matching the *PACKAGE*."
  (let ((data (or input (read-dat-file-for-package))))
    `(~>> (split ,separator ,data)
          ,@(when batches-of
              `((batches _ ,batches-of)))
          ,(if whole
               `(funcall ,item-parser)
               `(mapcar ,item-parser)))))

(defmacro defsummary (year day)
  (let* ((advent-url (fmt "https://adventofcode.com/~d/day/~d" year day))
         (part1-output (summarize (funcall (find-symbol "PART-1"))))
         (part2-output (summarize (funcall (find-symbol "PART-2")))))
    `(defsection ,(intern "@SUMMARY") (:title "Summary")
       ,(fmt "**Requirements:** [Day ~2,'0d](~a)~%" day advent-url)
       ,(fmt "**Part 1:**~%~A~%**Part 2:**~%~A~%" part1-output part2-output))))

(defmacro summarize (form)
  "Measure the real time to execute FORM and return a formatted string
 showing the result and the wall clock execution time."
  (with-unique-names (old-bytes new-bytes useconds result start end)
    `(let* ((,old-bytes (sb-ext:get-bytes-consed))
            (,start (get-internal-real-time))
            (,result ,form)
            (,end (get-internal-real-time))
            (,new-bytes (sb-ext:get-bytes-consed))
            (,useconds (/ (- ,end ,start) internal-time-units-per-second)))
       (fmt "> Time: ~7,3fms  Memory: ~7:dkb  Answer: ~10T~a~%"
            (* 1000 ,useconds) (floor (- ,new-bytes ,old-bytes) 1024) ,result))))
