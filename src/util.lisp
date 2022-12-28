(mgl-pax:define-package :aoc.util
  (:use :cl :mgl-pax)
  (:import-from :asdf #:system-relative-pathname)
  (:import-from :alexandria
                #:lastcar
                #:read-file-into-string
                #:symbolicate
                #:with-unique-names)
  (:import-from :cl-ppcre
                #:split
                #:regex-replace-all)
  (:import-from :serapeum
                #:~>>
                #:concat
                #:fmt))

(in-package :aoc.util)

(defsection @aoc.util (:title "Utilities")
  "A collection of helpful utilities for working on Advent of Code problems.
There are 3 utilities of primary interest:

1. DEFSUMMARY, which is responsible for generating the summary of a day
2. READ-DAY-INPUT, which is responsible for reading the input file for the day
3. SCAFFOLD, which fetches the input file and prepares the code file based on a template

SCAFFOLD is really only of interest at the REPL."
  (*aoc-session* variable)
  (defsummary macro)
  (find-section function)
  (parent-dir function)
  (read-day-input macro)
  (scaffold function))

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
  "Create a new lisp file for `YEAR` and `DAY` based on the `day.tmpl` template.
When *AOC-SESSION* is non-nil, download the input file
for the day to `dayNN.dat` in the directory for the appropriate year. An error will
be thrown if a directory matching `YEAR` does not exist."
  (scaffold-code year day)
  (unless (null *aoc-session*)
    (scaffold-input year day)))

(defmacro extract-date-from-string (string &body body)
  "Bind YEAR and DAY to values extracted from STRING by the regex
`(\\d{4}).*(\\d{2})` and run BODY in the scope of those bindings."
  `(cl-ppcre:register-groups-bind (,(intern "YEAR") ,(intern "DAY"))
       ("(\\d{4}).*(\\d{2})" ,string)
     ,@body))

(defun find-section (filename)
  "Find the section object corresponding with the supplied `FILENAME`.
Year and day for the file will be determined with a regex of the form:
`(\\d{4}).*(\\d{2})`. A filename not matching this pattern will error."
  (extract-date-from-string filename
    (let ((package-name (fmt "~d.~d" year day)))
      (symbol-value (find-symbol (concat "@" package-name) package-name)))))

(defun parent-dir (pathname)
  "Return the string naming the parent directory of `PATHNAME`."
  (lastcar (pathname-directory pathname)))

(defun read-dat-file-for-package (package)
  (extract-date-from-string (package-name package)
    (read-file-into-string (day-file year day))))

(defmacro read-day-input (item-parser &key (separator "\\n") (compact nil) (whole nil) (input nil))
  "Load the input data for the current day based on the name of *PACKAGE*, splitting
the file based on the value of SEPARATOR and processing each string with ITEM-PARSER.

If WHOLE is non-nil, skip splitting and pass to ITEM-PARSER directly instead of mapping.
If INPUT is supplied, use that instead of loading the DAT file matching the *PACKAGE*.
If COMPACT is non-nil, remove any NIL values after mapping over the data."
  (with-unique-names (data)
    `(let ((,data (or ,input (read-dat-file-for-package ,*package*))))
       ,(if whole
            `(funcall ,item-parser ,data)
            `(~>> (split ,separator ,data)
                  (mapcar ,item-parser)
                  ,@(when compact
                      `((remove nil))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro summarize (form &optional show-answer)
    "Measure the real time to execute FORM and return a formatted string
 showing the result and the wall clock execution time."
    (with-unique-names (old-bytes new-bytes useconds result start end)
      `(let* ((,old-bytes (sb-ext:get-bytes-consed))
              (,start (get-internal-real-time))
              (,result ,form)
              (,end (get-internal-real-time))
              (,new-bytes (sb-ext:get-bytes-consed))
              (,useconds (/ (- ,end ,start) internal-time-units-per-second)))
         (values (fmt "> Time: ~7,3fms  Memory: ~7:dkb  ~@[Answer: ~10T~a~]~%"
                      (* 1000 ,useconds) (floor (- ,new-bytes ,old-bytes) 1024)
                      (when ,show-answer ,result))
                 ,result))))

  (defun safe-summarize-funcall (string &optional arg show-answer)
    (let* ((symbol (find-symbol string))
           (fdefn (and (fboundp symbol) (fdefinition symbol))))
      (and fdefn (summarize (funcall fdefn arg) show-answer)))))

(defmacro defsummary ((&key title (show-answer t)) &body body)
  "Create a DEFSECTION for an Advent of Code puzzle that automatically includes a link to the
requirements for the day as well as performance measurements of the `BUILD-DATA`, `PART-1`, and
`PART-2` functions. All content within BODY is spliced directly into the body of the DEFSECTION
after the requirements and results are displayed.

TITLE should be the title of the day's problem. SHOW-ANSWER defaults to T as most problems
have simple and concise numeric or string answers. For more complicated problems whose output
might overflow a quote block, feel free to pass NIL instead."
  (extract-date-from-string (package-name *package*)
    (let* ((advent-url (fmt "https://adventofcode.com/~d/day/~d" year (parse-integer day)))
           (requirements (fmt "##### *Requirements*~%[Day ~2,'0d](~a)~%" day advent-url)))
      (multiple-value-bind (build-summary result) (safe-summarize-funcall "BUILD-DATA")
        (let* ((part1-summary (safe-summarize-funcall "PART-1" result show-answer))
               (part2-summary (safe-summarize-funcall "PART-2" result show-answer))
               (results (fmt "##### *Results*~%Input Parsing:~%~a~%Part 1:~%~a~%Part 2:~%~a~%~%"
                             build-summary part1-summary part2-summary)))
          `(defsection ,(symbolicate "@" year "." day) (:title ,title)
             ,requirements
             ,results
             "##### *Reflections*"
             ,@body))))))
