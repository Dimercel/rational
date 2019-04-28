#|
  This file is a part of rational project.
  Copyright (c) 2019 Ito Dimercel (xolcman@gmail.com)
|#

(defsystem "rational-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Ito Dimercel"
  :license "GNU"
  :depends-on ("rational"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "rational"))))
  :description "Test system for rational"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
