(cl:in-package #:elisp-format)

(declaim (inline new))
(defun new (class &rest args)
  (apply #'make-instance class args))

(defmacro while (test &rest body)
  `(cl:do () ((cl:not ,test) nil) ,@body))

(declaim (inline array-last))
(defun array-last (array)
  (aref array (1- (length array))))
