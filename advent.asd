(defsystem "advent"
  :version "0.1.0"
  :author "Brit Butler"
  :license "MIT"
  :depends-on ("alexandria" "serapeum" "cl-ppcre" "graph" "screamer" "mgl-pax")
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("2019" "2020" "2021" "overview"))
                 (:file "overview" :depends-on ("2019" "2020" "2021"))
                 (:file "util")
                 (:module "2021"
                  :pathname "2021"
                  :depends-on ("util")
                  :components
                  ((:file "day01")
                   (:file "day02")
                   (:file "day03")
                   (:file "day04")
                   (:file "day05")
                   (:file "day06")
                   (:file "day07")
                   (:file "day08")
                   (:file "day09")
                   (:file "day10")
                   (:file "day11")))
                 (:module "2020"
                  :pathname "2020"
                  :depends-on ("util")
                  :components
                  ((:file "day01")
                   (:file "day02")
                   (:file "day03")
                   (:file "day04")
                   (:file "day05")
                   (:file "day06")
                   (:file "day07")
                   (:file "day08")
                   (:file "day09")))
                 (:module "2019"
                  :pathname "2019"
                  :depends-on ("util")
                  :components
                  ((:file "day01"))))))
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
                 (:file "2020")
                 (:file "2019"))))
  :description "Test system for advent"
  :perform (test-op (op c) (symbol-call :rove :run c)))
