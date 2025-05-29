;; Parts of code in this file are adapted directly from Invistra:
;; Copyright © 2010-2023 Robert Strandh, Tarn W. Burton
;; For the details see the license file in the project directory

(in-package #:elisp-format)

(defclass  elisp-client (incless-extrinsic:extrinsic-client) ())

(defclass elisp-client-impl
    (elisp-client quaviver/schubfach:client)
  ())

(defvar *client* (make-instance 'elisp-client-impl))

(defun ensure-symbol (name &optional (package *package*))
  (intern (string name) package))

(defmacro define-interface ((client-var client-class &optional intrinsic)
                            &body body)
  (declare (ignore client-class))
  (let* ((intrinsic-pkg (if intrinsic `(find-package ,intrinsic) *package*))
         (format-func (ensure-symbol '#:format intrinsic-pkg))
         (initialize-func (ensure-symbol '#:initialize-elisp-format)))
    `(progn
       (defun ,format-func (control-string &rest args)
         (apply #'format ,client-var control-string args))

       (defmacro ,(ensure-symbol '#:elisp-format intrinsic-pkg) (control-string)
         (formatter ,client-var control-string))

       (define-compiler-macro ,format-func (&whole form control-string &rest args)
         (format-compiler-macro ,client-var form control-string args))

       (defun ,initialize-func ()
         ,@body))))
