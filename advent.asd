(defsystem "advent"
  :version "0.1.0"
  :author "Brit Butler"
  :license "MIT"
  :depends-on ("alexandria" "arrows" "cl-ppcre" "graph" "screamer")
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "util")
                 (:module "2021"
                  :pathname "2021"
                  :components
                  ((:file "day01")))
                 (:module "2020"
                  :pathname "2020"
                  :components
                  ((:file "day01")
                   (:file "day02")
                   (:file "day03")
                   (:file "day04")
                   (:file "day05")
                   (:file "day06")
                   (:file "day07")
                   (:file "day08")
                   (:file "day09"))))))
  :description "Advent of Code solutions"
  :in-order-to ((test-op (test-op "advent/tests"))))

(defsystem "advent/tests"
  :author "Brit Butler"
  :license "MIT"
  :depends-on ("advent"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "2021")
                 (:file "2020"))))
  :description "Test system for advent"
  :perform (test-op (op c) (symbol-call :rove :run c)))
