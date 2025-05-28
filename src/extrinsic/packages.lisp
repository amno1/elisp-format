(defpackage #:elisp-format-extrinsic
  (:use #:common-lisp #:elisp-format)
  (:shadow #:format
           #:formatter)
  (:export #:*client*
           #:extrinsic-client
           #:format
           #:formatter
           #:initialize-invistra))

