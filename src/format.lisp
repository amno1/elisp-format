(in-package #:elisp-format)

(defvar *destination*)

(defun format (client control &rest args)
  (let ((*destination* (make-string-output-stream)))
    (invistra::with-arguments args
      (invistra::format-with-runtime-arguments client control))
    (get-output-stream-string *destination*)))

