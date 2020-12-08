(defsystem "advent2020"
  :version "0.1.0"
  :author "Brit Butler"
  :license "MIT"
  :depends-on ("alexandria" "arrows" "cl-ppcre" "graph")
  :components ((:module "src"
                :serial t
                :components
                ((:file "main")
                 (:file "util")
                 (:file "day01")
                 (:file "day02")
                 (:file "day03")
                 (:file "day04")
                 (:file "day05")
                 (:file "day06")
                 (:file "day07"))))
  :description ""
  :in-order-to ((test-op (test-op "advent2020/tests"))))

(defsystem "advent2020/tests"
  :author "Brit Butler"
  :license "MIT"
  :depends-on ("advent2020"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for advent2020"
  :perform (test-op (op c) (symbol-call :rove :run c)))
