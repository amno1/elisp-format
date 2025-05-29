;; integer printers

;; Parts of code in this file are adapted directly from Invistra:
;; Copyright © 2010-2023 Robert Strandh, Tarn W. Burton
;; For the details see the license file in the project directory

(in-package #:elisp-format)

(defclass elisp-radix-directive (elisp-directive)
  ((case
    :initarg :case :initform nil :type symbol :accessor parameter-case)
   (radix
    :initarg :radix :type integer :accessor parameter-radix)))

(defmethod argument-to-string ((parameter elisp-radix-directive))
  (let* ((value (directive-argument parameter))
         (radix (parameter-radix parameter))
         (prefix (argument-prefix parameter))
         (prefix-length (length prefix))
         (precision (argument-precision parameter)))
    (declare (type fixnum radix))
    (and (= radix 8) precision (= prefix-length 1) (zerop precision) (zerop value)
         (return-from argument-to-string "0"))
    (when (minusp value)
      (setf (slot-value parameter 'sign-char) #\-))
    (setf value (if (floatp value) (abs (floor value)) (abs value)))
    (let* ((*print-base* radix)
           (*print-radix* nil)
           (print-case (parameter-case parameter)))
      (let* ((digit-count (quaviver.math:count-digits radix value)))
        (declare (type fixnum digit-count radix value))
        (and (= radix 8) (= prefix-length 1) (incf digit-count))
        (with-output-to-string (stream)
          (cond
            ((and (= radix 8) precision (<= precision digit-count))
             (princ prefix stream))
            (t
             (princ prefix stream)))
          (and precision (> precision digit-count)
               (loop repeat (- precision digit-count)
                     do (write-char #\0 stream)))
          (princ value stream)
          (case print-case
            (:upper
             (return-from argument-to-string
               (string-upcase (get-output-stream-string stream))))
            (:lower
             (return-from argument-to-string
               (string-downcase (get-output-stream-string stream))))))))))

;; (defclass b-elisp-directive (elisp-directive) ())

;; (defmethod specialize-directive
;;     (client (char (eql #\b)) directive end-directive)
;;   (declare (ignore client end-directive))
;;   (change-class directive 'b-elisp-directive))

;; (defmethod interpret-item (client (directive b-elisp-directive) &optional args)
;;   (declare (ignore client args))
;;   (change-class directive 'elisp-radix-directive :radix 2 :prefix "")
;;   (print-arg directive *destination*))

;; d-directive

(defclass d-directive (elisp-directive) ())

(defmethod specialize-directive
    (client (char (eql #\d)) directive end-directive)
  (declare (ignore client end-directive))
  (change-class directive 'd-directive :parameters nil))

(defmethod interpret-item (client (directive d-directive) &optional args)
  (declare (ignore client args))
  (change-class
   directive 'elisp-radix-directive
   :prefix ""
   :radix 10)
  (print-arg directive *destination*))


;; o-directive

(defclass o-directive (elisp-directive) ())

(defmethod specialize-directive
    (client (char (eql #\o)) directive end-directive)
  (declare (ignore client end-directive))
  (change-class directive 'o-directive :parameters nil))

(defmethod interpret-item (client (directive o-directive) &optional args)
  (declare (ignore client args))
  (change-class
   directive
   'elisp-radix-directive
   :radix 8
   :prefix
   (if (and (argument-prefix directive) (/= (directive-argument directive) 0))
       "0" ""))
  (print-arg directive *destination*))


;; x-directives

(defclass |x-directive| (elisp-directive) ())

(defmethod specialize-directive
    (client (char (eql #\x)) directive end-directive)
  (declare (ignore client end-directive))
  (change-class directive '|x-directive| :parameters nil))

(defmethod interpret-item (client (directive |x-directive|) &optional args)
  (declare (ignore client args))
  (change-class
   directive 'elisp-radix-directive
   :radix 16
   :case :lower
   :prefix
   (if (and (argument-prefix directive) (/= (directive-argument directive) 0))
       "0x" ""))
  (print-arg directive *destination*))

(defclass |X-directive| (elisp-directive) ())

(defmethod specialize-directive
    (client (char (eql #\X)) directive end-directive)
  (declare (ignore client end-directive))
  (change-class directive '|X-directive| :parameters nil))

(defmethod interpret-item (client (directive |X-directive|) &optional args)
  (declare (ignore client args))
  (change-class
   directive 'elisp-radix-directive
   :radix 16
   :case :upper
   :prefix
   (if (and (argument-prefix directive) (/= (directive-argument directive) 0))
       "0X" ""))
  (print-arg directive *destination*))
