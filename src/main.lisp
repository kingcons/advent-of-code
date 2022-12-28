(mgl-pax:define-package :aoc
  (:use :cl :mgl-pax :aoc.util)
  (:import-from :alexandria
                #:lastcar
                #:symbolicate)
  (:import-from :serapeum
                #:assort
                #:fmt))

(in-package :aoc)

(defsection @links (:title "Links")
  "Here is the [github repo][repo] and
   here is the [website][site].

   [repo]: https://github.com/kingcons/advent-of-code
   [site]: https://kingcons.github.io/advent-of-code")

(defsection @background (:title "Background")
  "> I have always aspired to a more spacious form
that would be free from the claims of poetry or prose
and would let us understand each other without exposing
the author or reader to sublime agonies.

- Czeslaw Milosz, _Ars Poetica_

What do we want from a program? For a long time, I have
wondered what it would mean to have truly [readable programs][rp].
I agree with Peter Seibel that [code is not literature][cr] and yet,
I [cannot help but imagine][rg] a world where software supports our understanding.

Source code, [like language][ll], is a weak medium for humans. Many of the things we care
about cannot be captured in source code:

- the shape and volume of runtime data as well as the course it takes through the program
- the performance characteristics of the application as it executes on physical hardware
- the domain knowledge and engineering constraints that influenced design decisions

Automated testing can capture some small fragments of these details but too much is left
to us to reconstruct using our imagination and experience. This is a large part of why
gaining comfort with a new codebase doesn't come from passive reading but active rewriting.

I continue to believe that the best way to combat these limitations comes from live environments
that allow runtime inspection of in-memory structures and support for inspecting, redefining,
and migrating both code and data in a live environment. I also recognize that, unlike the
fantasies of my youth, most people have no interest in looking under the hood at how a program
functions or how the machine carries out its tasks. And industry will continue moving away from
[systems that are entirely comprehensible][dp] by a single individual.

This program won't overcome these limitations in understanding. It will not reveal all its
contextual mysteries and the baggage of its creation to those who read it or run it. But I
enjoy programming most when it is a creative writing exercise. So I will try to make this
program open to exploration, document a little of what I understand, and show some
compassion for myself and you, dear reader.

[cr]: https://gigamonkeys.com/code-reading/
[rp]: https://lukego.github.io/blog/2012/10/24/readable-programs/
[rg]: https://blog.kingcons.io/posts/Research-Goals.html
[ll]: https://blog.kingcons.io/posts/For-Posterity.html
[dp]: https://www.cs.virginia.edu/~evans/cs655/readings/smalltalk.html")

(defsection @structure (:title "Structure")
  "Readers will note that each day has some common themes:

* A package (or namespace) is defined for each problem.
* We begin with a [DEFSUMMARY][aoc.util:defsummary] block to capture reflections.
* Three functions will always be present: `BUILD-DATA`, `PART-1`, and `PART-2`.
* `BUILD-DATA` will always rely on [READ-DAY-INPUT][aoc.util:read-day-input] and some custom logic.

Each function has a distinct role:

1. `BUILD-DATA`, responsible for parsing the input into a usable representation
2. `PART-1`, responsible for solving the first part using parsed input
3. `PART-2`, responsible for solving the second part using parsed input

These three functions have optional inputs to default to the supplied data but simplify
mocking in tests. Tests are in a separate ASDF system and rely only on these three functions.")

;;; NOTE: The following code exists to allow autogenerating both an exhaustive
;;; performance and results summary for all completed exercises as well as the
;;; rollup sections for each individual year of Advent of Code. While the
;;; metaprogramming is a bit gross and overreaching, the ability to just add
;;; a new day to the ASDF system and start hacking without linking to sections
;;; in 3 different places is considered worth the suffering. This code should
;;; only very rarely be modified. Note the years are still hardcoded in @ADVENT.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-year-section (year)
    (let* ((year-string (parent-dir (first year)))
           (year-section (symbolicate "@AOC." year-string)))
      (flet ((build-section-from-pathname (pathname)
               (let ((section (find-section (namestring pathname))))
                 (list (section-name section) 'section))))
        `(defsection ,year-section (:title ,(fmt "Advent ~a" year-string))
           ,@(loop for file in year
                   collecting (build-section-from-pathname file)))))))

(defmacro generate-years ()
  (flet ((advent-file? (x)
           (search "day" (pathname-name x) :test #'string=)))
    (let* ((components (mapcar #'asdf:component-pathname (asdf:required-components :advent)))
           (advent-components (remove-if-not #'advent-file? components))
           (years (assort advent-components :key #'parent-dir :test #'string=)))
      `(progn
         ,@ (loop for year in years
                  collect (generate-year-section year))))))

(generate-years)

(defsection @advent (:title "Advent of Code")
  (@links section)
  (@background section)
  (@structure section)
  (@aoc.util section)
  (@aoc.2022 section)
  (@aoc.2021 section)
  (@aoc.2020 section)
  (@aoc.2019 section))

(defun build-site ()
  (let ((*document-normalize-packages* t))
    (update-asdf-system-html-docs
     @advent :advent
     :target-dir (asdf:system-relative-pathname :advent "docs/")
     :pages `((:objects (,aoc:@advent)
               :source-uri-fn ,(make-github-source-uri-fn
                                :advent "https://github.com/kingcons/advent-of-code"))))))
