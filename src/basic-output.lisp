(in-package #:elisp-format)

(defmethod argument-to-string ((directive elisp-directive))
  (let* ((string
           (with-output-to-string (s)
             (if (pretty-print directive)
                 (princ (directive-argument directive) s)
                 (prin1 (directive-argument directive) s))))
         (length (length string))
         (precision (argument-precision directive)))
    (if (>= (length string) 0)
        (if precision
            (subseq string 0 (if (> precision 0)
                                 (min precision length)
                                 0))
            string))))

(defun print-arg (directive destination)
  "Print value of DIRECTIVE to DESTINATION stream.

Print sign character and right or left padding.
The value of an argument itself is printed to a string in
`argument-to-string' specialisations."
  (let* ((string (argument-to-string directive))
         (sign (sign-char directive))         
         (length (length string))
         (width  (argument-width directive))
         (pad-char (argument-padchar directive))
         (pad-length (max 0 (- width length))))
    (cond
      ((pad-right-p directive)
       (setf pad-char #\Space)
       (when sign
         (write-char sign destination))
       ;; Print the string in reverse order
       (loop for index downfrom (1- length) to 0
             for c across string
             do  (write-char c destination))
       (loop repeat pad-length
             do (write-char pad-char destination)))
      (t ; pad-left
       (cond ((eql pad-char #\0)
              (when sign
                (write-char sign destination))
              (loop repeat pad-length
                    do (write-char pad-char destination)))
             (t
              (loop repeat pad-length
                    do (write-char pad-char destination))
              (when sign
                (write-char sign destination))))
       ;; Print the string in reverse order
       (loop for index downfrom (1- length) to 0
             for c across string
             do (write-char c destination))))))


;; %-directive

(defclass %-directive (elisp-directive) ())

(defmethod specialize-directive
    ((client elisp-client) (char (eql #\%)) (directive elisp-directive) end-directive)
  (declare (ignore client end-directive))
  (change-class directive '%-directive :parameters nil))

(defmethod parameter-specifications (client (directive %-directive))
  (declare (ignore client directive)))

(defmethod interpret-item (client (directive %-directive)
                           &optional parameters)
  (declare (ignore parameters))
  (write-char #\% *destination*))

(defmethod compile-item (client (directive %-directive)
                         &optional parameters)
  (declare (ignore parameters))
  `((write-char #\% *destination*)))


;; c-directive

(defclass c-directive (elisp-directive) ())

(defmethod specialize-directive
    ((client elisp-client) (char (eql #\c)) (directive elisp-directive) end-directive)
  (declare (ignore client end-directive))
  (change-class directive 'c-directive :pretty-print t :parameters nil))

(defmethod interpret-item (client (directive c-directive)
                           &optional parameters)
  (declare (ignore parameters))
  (let ((char (directive-argument directive)))
    (when (integerp char)
      (setf (directive-argument directive) (code-char char)))
    (print-arg directive *destination*)))

(defmethod compile-item (client (directive c-directive) &optional parameters)
  (declare (ignore parameters))
  (let ((char (directive-argument directive)))
    (when (integerp char)
      (setf (directive-argument directive) (code-char char)))
    `((print-arg ,directive *destination*))))

(defclass |s-directive| (elisp-directive) ())

(defmethod specialize-directive
    (client (char (eql #\s)) directive end-directive)
  (declare (ignore client end-directive))
  (change-class directive '|s-directive| :pretty-print t :parameters nil))

(defmethod interpret-item (client (directive |s-directive|) &optional args)
  (declare (ignore args))
  (let ((arg (directive-argument directive)))
    (typecase arg
      (character
       (setf (directive-argument directive) (char-code arg)))))
  (print-arg directive *destination*))

(defclass |S-directive| (elisp-directive) ())

(defmethod specialize-directive
    (client (char (eql #\S)) directive end-directive)
  (declare (ignore client end-directive))
  (change-class directive '|S-directive| :parameters nil))

(defmethod interpret-item (client (directive |S-directive|) &optional args)
  (declare (ignore args))
  (let ((arg (directive-argument directive)))
    (when (characterp arg)
     (setf (directive-argument directive) (char-code arg))))
  (print-arg directive *destination*))

;;; basic-output.lisp ends here
