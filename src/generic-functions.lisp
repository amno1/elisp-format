(cl:in-package #:elisp-format)

;;; Print an argument to string
(defgeneric argument-to-string (directive)
  (:method (directive)
    (declare (ignore directive))))

;;; Parse a control string into a directive object
(defgeneric parse-directive (control-string start)
  (:method (control-string start)
    (declare (ignore control-string start))))
