(in-package #:asdf-user)

(defsystem #:ann
  :version (:read-file-line "version.txt" :at 0)
  :description "A simple text annotation assistant."
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :depends-on (#:rutilsx #:cl-ppcre #:hunchentoot #:cl-who #:cl-fad
               #:yason #:cl-yaml #:cl-json
               #+dev #:should-test)
  :serial t
  :components
  ((:module "src" :serial t
    :components (#+dev (:file "../hunch")
                 (:file "packages")
                 (:file "ann")
                 (:module "formats" :serial t
                  :components ((:file "bsf")))
                 (:file "diff")
                 (:file "site")))
   #+dev
   (:module "test" :serial t
    :components ())))
