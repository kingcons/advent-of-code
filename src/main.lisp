(defpackage aoc
  (:use :cl :mgl-pax)
  (:export :@advent))

(in-package :aoc)

(defsection @advent (:title "Advent of Code")
  (@links section)
  (@aoc.2019 section)
  (@aoc.2020 section)
  (@aoc.2021 section))

(defsection @links (:title "Links")
  "Here is the [github repo][repo] and
   here is the [website][site].

   [repo]: https://github.com/kingcons/advent-of-code
   [site]: https://kingcons.github.io/advent-of-code")

(defsection @aoc.2019 (:title "Advent 2019")
  (2019.01:@2019.01 section))

(defsection @aoc.2020 (:title "Advent 2020"))

(defsection @aoc.2021 (:title "Advent 2021"))

(defun build-site ()
  (let ((*document-normalize-packages* nil))
    (update-asdf-system-html-docs
     @advent :advent
     :target-dir (asdf:system-relative-pathname :advent "docs/")
     :pages `((:objects (,aoc:@advent)
               :source-uri-fn ,(make-github-source-uri-fn
                                :advent "https://github.com/kingcons/advent-of-code"))))))
