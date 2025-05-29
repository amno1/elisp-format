(defpackage #:elisp-format
  (:use #:common-lisp #:invistra)
  (:shadow #+sbcl
           #:while
           #:format
           #:formatter)
  (:export #:*client*
           #:elisp-client
           #:format
           #:formatter
           #:initialize-invistra))

;;; packages.lisp ends here
