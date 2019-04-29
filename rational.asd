#|
  This file is a part of rational project.
  Copyright (c) 2019 Ito Dimercel (xolcman@gmail.com)
|#

#|
  Author: Ito Dimercel (xolcman@gmail.com)
|#

(defsystem "rational"
  :version "0.1.0"
  :author "Ito Dimercel"
  :license "GNU"
  :depends-on ("smug")
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "core")
                 (:file "parser" :depends-on ("utils" "core"))
                 (:file "rational" :depends-on ("core" "parser"))
                )))
  :description "Basic calculator for rational numbers"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "rational-test"))))
