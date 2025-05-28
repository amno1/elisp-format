(cl:in-package #:asdf-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :emacs-lisp-format.system)
    (defpackage :emacs-lisp-format.system
      (:use :common-lisp :asdf))))

(in-package :emacs-lisp-format.system)

(defsystem :elisp-format 
  :description "Emacs Lisp format implementation" 
  :author "Arthur Miller <arthur.miller@live.com>" 
  :licence "GPL" 
  :version "0.0.1"
  :depends-on (invistra-custom-extrinsic
               quaviver/native)
  :components ((:module src
                :serial t
                :components ((:file "packages")
                             (:file "utilities")
                             (:file "generic-functions")
                             (:file "directive")
                             (:file "format")
                             (:file "interface")
                             (:file "basic-output")
                             (:file "radix-control")
                             (:file "floating-point-printers")))))

(defsystem :elisp-format-extrinsic
  :description "Extrinsic interface to Elisp format implementation" 
  :author "Arthur Miller <arthur.miller@live.com>" 
  :licence "GPL" 
  :version "0.0.1"
  :depends-on (elisp-format)
  :components ((:module src/extrinsic
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))

(defsystem :elisp-format-extrinsic-test 
  :description "Tests for Emacs Lisp format function" 
  :author "Arthur Miller <arthur.miller@live.com>" 
  :licence "GPLv3" 
  :version "0.0.1"
  :depends-on (elisp-format-extrinsic parachute)
  :components ((:module "src/extrinsic/test"
                :serial t
                :components
                ((:file "packages")
                 (:file "tools")
                 (:file "tests")))))
