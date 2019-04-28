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
                ((:file "rational" :depends-on ("utils"))
                 (:file "utils"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "rational-test"))))
