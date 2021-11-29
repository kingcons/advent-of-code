(defpackage aoc
  (:use :cl :mgl-pax)
  (:export :@advent))

(in-package :aoc)

(defsection @advent (:title "Advent of Code")
  (@links section)
  (@background section)
  (@summary section)
  (@aoc.2019 section)
  (@aoc.2020 section)
  (@aoc.2021 section))

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

(defsection @summary (:title "Current Progress")
  "I do not hold myself to completing every exercise and oscillate between striving
for a concise or clever implementation versus an optimized one. Still, it's useful to
have an overview of what code has been written and how it performs so I will add that here."
  (aoc.overview:@overview section))

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
