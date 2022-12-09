(mgl-pax:define-package :aoc.util
  (:use :cl :mgl-pax)
  (:import-from :asdf #:system-relative-pathname)
  (:import-from :alexandria #:read-file-into-string
                            #:symbolicate
                            #:with-unique-names)
  (:import-from :cl-ppcre #:split
                          #:regex-replace-all)
  (:import-from :serapeum #:~>>
                          #:batches
                          #:fmt))

(in-package :aoc.util)

(defsection @aoc.util (:title "Useful Utilities")
  (*aoc-session* variable)
  (defsummary macro)
  (extract-date-from-string macro)
  (read-day-input macro)
  (scaffold function)
  (summarize macro))

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

(defun day-file (year day &key (extension "dat"))
  (let ((filename (fmt "src/~d/day~2,'0d.~a" year day extension)))
    (system-relative-pathname :advent filename)))

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

(defmacro extract-date-from-string (string &body body)
  "Bind YEAR and DAY to values extracted from STRING by the regex
`(\\d{4}).*(\\d{2})` and run BODY in the scope of those bindings."
  `(cl-ppcre:register-groups-bind (,(intern "YEAR") ,(intern "DAY"))
       ("(\\d{4}).*(\\d{2})" ,string)
     ,@body))

(defun read-dat-file-for-package (package)
  (extract-date-from-string (package-name package)
    (read-file-into-string (day-file year day))))

(defmacro read-day-input (item-parser &key (separator "\\n") (compact nil)
                         (batches-of nil) (whole nil) (input nil))
  "Load the input data for the current day based on the name of *PACKAGE*, splitting
the file based on the value of SEPARATOR and processing each string with ITEM-PARSER.
If BATCHES-OF is supplied, divide the separated data into chunks of the desired size.
If WHOLE is non-nil, after splitting pass to ITEM-PARSER directly instead of mapping.
If INPUT is supplied, use that instead of loading the DAT file matching the *PACKAGE*.
If COMPACT is non-nil, remove any NIL values after mapping over the data."
  (with-unique-names (data)
    `(let ((,data (or ,input (read-dat-file-for-package ,*package*))))
       (~>> (split ,separator ,data)
            ,@(when batches-of
                `((batches _ ,batches-of)))
            ,(if whole
                 `(funcall ,item-parser)
                 `(mapcar ,item-parser))
            ,@(when compact
                `((remove nil)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun safe-summarize-funcall (string &rest args)
    (let* ((symbol (find-symbol string))
           (fdefn (and (fboundp symbol) (fdefinition symbol))))
      (and fdefn (summarize (apply 'funcall fdefn args))))))

(defmacro defsummary ((&key title) &body body)
  (extract-date-from-string (package-name *package*)
    (let* ((advent-url (fmt "https://adventofcode.com/~d/day/~d" year (parse-integer day)))
           (requirements (fmt "**Requirements:** [Day ~2,'0d](~a)~%" day advent-url))
           (part1-output (safe-summarize-funcall "PART-1"))
           (part2-output (safe-summarize-funcall "PART-2"))
           (header (fmt "~a~%**Part 1:**~%~a~%**Part 2:**~%~a~%~%"
                        requirements part1-output part2-output)))
      `(defsection ,(symbolicate "@" year "." day) (:title ,title)
         "---"
         ,header
         "---"
         "##### *Reflections*"
         ,@body))))

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
