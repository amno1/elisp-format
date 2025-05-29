;;; Floating-point printers

;; this file has some code adapted directly from Invistra:
;; Copyright © 2010-2023 Robert Strandh, Tarn W. Burton


(in-package #:elisp-format)

(declaim (inline ensure-float))
(defun ensure-float (value)
  (coerce value 'double-float))

(defclass elisp-float-directive (elisp-directive)
  ((exponent
    :initarg :exponent :type (or null integer) :initform 0 :accessor
    argument-exponent)
   (e
    :initarg :e :type (or null integer) :initform 1 :accessor argument-e)
   (k
    :initarg :k :type (or null integer) :initform 0 :accessor argument-k)
   (significand
    :initarg :significand :type (or null integer) :accessor argument-significand)
   (digits
    :initarg :digits :type (or null integer) :initform nil :accessor argument-digits)
   (exponentchar
    :initarg :exponent-char :type character :initform #\e :accessor exponent-char)
   (overflowchar
    :initarg :overflow-char :type (or null character) :initform nil :accessor overflow-char)))

(defun print-float-arg (directive printer)
  (let ((value (directive-argument directive)))
    (if (or (complexp value)
            (and (floatp value)
                 #+abcl
                 (or (system:float-infinity-p value)
                     (system:float-nan-p value))
                 #+allegro
                 (or (excl:infinityp value)
                     (excl:nanp value))
                 #+ccl
                 (ccl::nan-or-infinity-p value)
                 #+(or clasp cmucl ecl)
                 (or (ext:float-infinity-p value)
                     (ext:float-nan-p value))
                 #+mezzano
                 (or (mezzano.extensions:float-infinity-p value)
                     (mezzano.extensions:float-nan-p value))
                 #+sbcl (or (sb-ext:float-infinity-p value)
                            (sb-ext:float-nan-p value)))
            (not (numberp value)))
        (let ((*print-base* 10)
              (*print-escape* nil)
              (*print-readably* nil))
          (write value :stream *destination*))
        (let ((client *client*)
              (value (ensure-float value)))
          (multiple-value-bind (significand exponent sign)
              (quaviver:float-triple client 10 value)
            (setf (slot-value directive 'sign-char)
                  (cond ((minusp sign) #\-)
                        ((print-sign-p directive) #\+)))
            (funcall printer
                     value
                     significand
                     exponent
                     (argument-width directive)
                     (argument-precision directive)
                     (argument-k directive)
                     (argument-e directive)
                     (overflow-char directive)
                     (exponent-char directive)
                     (argument-prefix directive)))))))

(defun round-away-from-zero (x n)
  (multiple-value-bind (q r)
      (truncate x n)
    (if (>= (* 2 r) n)
        (1+ q)
        q)))

(defun trim-fractional (significand digit-count fractional-position d)
  (declare (type fixnum significand))
  (let ((l (max 0 (- digit-count fractional-position))))
    (cond ((< l d)
           (if (zerop significand)
               (setf fractional-position (- d))
               (setf significand
                     (* significand
                        (expt 10
                              (+ (max 0
                                      (- fractional-position digit-count))
                                 (- d l))))
                     digit-count (quaviver.math:count-digits 10 significand))))
          ((> l d)
           (when (minusp fractional-position)
             (setf fractional-position
                   (max fractional-position (- 1 d))))
           (setf significand (round-away-from-zero significand
                                                   (expt 10 (- l d)))
                 digit-count (quaviver.math:count-digits 10 significand)))))
  (values significand digit-count fractional-position))


;; %f Fixed-format floating point.

(defclass f-directive (elisp-float-directive) ())

(defmethod argument-to-string ((directive f-directive))
  (with-output-to-string (stream)
    (let ((*destination* stream))
      (print-float-arg directive #'print-fixed-arg))))

(defun print-fixed-arg (value significand exponent
                        w d k e overflowchar exponentchar trailing-dot)
  (declare (ignore e exponentchar)
           (type fixnum significand))
  (let* ((digit-count (quaviver.math:count-digits 10 significand))
         (fractional-position (if (zerop significand)
                                  0
                                  (+ digit-count k exponent)))
         (leading-zeros 0)
         (my-significand significand))
    (declare (type fixnum my-significand))
    (flet ((compute-width ()
             (+ 1
                leading-zeros
                (max digit-count fractional-position)
                (- (min 0 fractional-position)))))
      (when (and w
                 (null d)
                 (> (compute-width) w))
        (multiple-value-setq (my-significand digit-count fractional-position)
          (trim-fractional my-significand digit-count fractional-position
                           (min (max 0 (- digit-count fractional-position))
                                (max 0
                                     (- w
                                        (max 0 fractional-position)
                                        1)))))
        (when (zerop my-significand)
          (setf fractional-position 1)))
      (when d
        (multiple-value-setq (my-significand digit-count fractional-position)
          (trim-fractional my-significand digit-count fractional-position d)))
      (when (and (>= fractional-position digit-count)
                 (null d)
                 (or (null w)
                     (null overflowchar)
                     (< (compute-width) w)))
        (if (zerop my-significand)
            (decf fractional-position)
            (setf my-significand (* my-significand
                                    (expt 10
                                          (+ fractional-position
                                             (- digit-count)
                                             1)))
                  digit-count (quaviver.math:count-digits 10 my-significand))))
      (when (and (not (plusp fractional-position))
                 (< value (expt 10 (- k)))
                 (or (null w) (null d)
                     (> w (1+ d)))
                 (or (null w)
                     (< (compute-width) w)))
        (setf leading-zeros 1))
      (cond ((or (null w)
                 (null overflowchar)
                 (<= (compute-width) w))
             (cond ((< fractional-position 0)
                    (setf fractional-position (1+ fractional-position)
                          leading-zeros 1))
                   ((= fractional-position 0)
                    (setf leading-zeros 1)))
             (cond ((= d 0)
                    (quaviver:write-digits 10 my-significand *destination*
                                           ;:leading-zeros leading-zeros
                                           ))
                   (t
                    (quaviver:write-digits 10 my-significand *destination*
                                           :leading-zeros leading-zeros
                                           :fractional-position fractional-position
                                           :fractional-marker #\.)))             
             nil)
            (t             
             (loop repeat w
                   do (write-char overflowchar *destination*))
             t))
      (when (and trailing-dot (>= fractional-position 0 ))
        (write-char #\. *destination*)))))

(defmethod specialize-directive
    (client (char (eql #\f)) directive end-directive)
  (declare (ignore client end-directive))
  (change-class
   directive 'f-directive :parameters nil
   :overflow-char (when (pad-right-p directive) (argument-padchar directive))))

(defmethod interpret-item (client (directive f-directive) &optional parameters)
  (declare (ignore parameters))
  (print-arg directive *destination*))

;; (defmethod compile-item (client (directive f-directive) &optional parameters)
;;   `((print-float-arg ,(incless:client-form client)
;;                      (lambda (client value digits exponent sign)
;;                        (print-fixed-arg client value digits exponent sign
;;                                         ,(colon-p directive) ,(at-sign-p directive)
;;                                         ,@parameters)))))


;; %e Exponential floating point.

(defclass e-directive (elisp-float-directive) nil)

(defmethod specialize-directive
    (client (char (eql #\e)) directive (end-directive t))
  (change-class directive 'e-directive :parameters nil))

(defmethod argument-to-string ((directive e-directive))
  (with-output-to-string (s)
    (let ((*destination* s))
      (print-float-arg directive #'print-exponent-arg))))

(defun print-exponent-arg (value significand exponent
                           w d e k overflowchar exponentchar trailing-dot)
  (declare (type fixnum significand))
  (let* ((digit-count (quaviver.math:count-digits 10 significand))
         (fractional-position k)
         (leading-zeros 0)
         (my-significand significand)
         (my-exponent (if (zerop significand)
                          0
                          (+ exponent digit-count (- k)))))
    (declare (type fixnum my-exponent))
    (let* ((exp-count (quaviver.math:count-digits 10 (abs my-exponent)))
           (leading-exp-zeros (1+ (- (or e exp-count) exp-count))))
      (when (= leading-exp-zeros 0) (incf leading-exp-zeros))
      (flet ((compute-width ()
               (+ 3
                  leading-zeros
                  (max digit-count fractional-position)
                  (- (min 0 fractional-position))
                  leading-exp-zeros
                  exp-count)))
        (when d
          (multiple-value-setq (my-significand digit-count fractional-position)
            (trim-fractional my-significand digit-count fractional-position
                             (cond ((zerop k)
                                    d)
                                   ((plusp k)
                                    (- d k -1))
                                   (t
                                    (+ d k 1))))))
        (when (and w
                   (null d)
                   (> (compute-width) w))
          (multiple-value-setq (my-significand digit-count fractional-position)
            (trim-fractional my-significand digit-count fractional-position
                             (max 0
                                  (- w
                                     (max 0 fractional-position)
                                     3
                                     exp-count)))))
        (when (and (= fractional-position digit-count)
                   (null d)
                   (or (null w)
                       (< (compute-width) w)
                       #+(or)(null d)
                       #+(or)(> w (1+ d))))
          (if (zerop significand)
              (setf fractional-position 1)
              (setf my-significand (* 10 my-significand)
                    digit-count (1+ digit-count))))
        (when (or (zerop significand)
                  (and (not (plusp fractional-position))
                       (or (null w)
                           (< (compute-width) w))))
          (setf leading-zeros 1
                fractional-position (1+ fractional-position)))
        (cond ((or (null w)
                   (null overflowchar)
                   (<= (compute-width) w))
               (quaviver:write-digits 10 my-significand *destination*
                                      :leading-zeros leading-zeros
                                      :fractional-position fractional-position
                                      :fractional-marker
                                      (when (or (> d 0) trailing-dot) #\.))
               (write-char (or exponentchar
                               (if (typep value *read-default-float-format*)
                                   #+abcl #\E #-abcl #\e
                                   (etypecase value
                                     (short-float #+abcl #\S #-abcl #\s)
                                     #-sbcl
                                     (single-float #+abcl #\F #-abcl #\f)
                                     (double-float #+abcl #\D #-abcl #\d)
                                     (long-float #+abcl #\L #-abcl #\l))))
                           *destination*)
               (write-char (if (minusp my-exponent) #\- #\+) *destination*)
               (quaviver:write-digits 10 (abs my-exponent) *destination*
                                      :leading-zeros leading-exp-zeros))
              (t
               (loop repeat w
                     do (write-char overflowchar *destination*))))))))

(defmethod interpret-item (client (directive e-directive) &optional parameters)
  (declare (ignore parameters))
  (print-arg directive *destination*))

;; (defmethod compile-item (client (directive e-directive) &optional parameters)
;;   `((print-float-arg ,(incless:client-form client)
;;                      (lambda (client value digits exponent sign)
;;                        (print-exponent-arg client value digits exponent sign
;;                                            ,(colon-p directive) ,(at-sign-p directive)
;;                                            ,@parameters)))))


;; %g General floating point.

(defclass quaviver-client (quaviver/native:client) ())
(defclass g-directive (elisp-float-directive) ())

(defvar *quaviver-native-client* (new 'quaviver-client))

(defmethod specialize-directive
    ((client t) (char (eql #\g)) directive (end-directive t))
  (let* ((v (directive-argument directive))
         (p (argument-precision directive)))
    (multiple-value-bind (significand dp sign)
        (quaviver:float-triple client 10 (coerce v 'float))
      (declare (type fixnum significand)
               (ignore sign))
      (let ((dc (quaviver.math:count-digits 10 significand)))
        (when (and (integerp v) (<= dc p))
          (return-from specialize-directive
            (change-class directive 'd-directive :parameters nil :precision 0)))
        (let* ((k 0)
               (dp (abs dp))
               (sp (- dc dp))
               (e (round (log v)))
               (exp (cond
                      ((and (= dc 1) (> dp (1+ p))) dp)
                      ((> dc e) 0)
                      ((= dc e) dp)
                      ((= sp dp) dp)
                      ((<= 1 sp p) 0)
                      ((<= sp dc p) 0)
                      ((> sp p) (- 1 dc))
                      ((= dp p) 0)
                      ((= dc p) 0)
                      ((> dp p) dp)
                      (t (+ dp p)))))
          (cond
            ((< -4 exp p)
             (cond
               ((> dc p)
                (cond
                  ((and (> dp p) (= exp 0))
                   (cond ((= e 0) (setf p 0))
                         ((> e sp) (setf p 0))
                         (t (setf p (- p sp)))))
                  ((= dc sp)
                   (setf p 0))
                  ((>= dp p)
                   (setf p (if (= (round (log v)) 0) (- p sp) 0)))
                  (t
                   (setf p (if (= dp p)
                               0
                               (- p sp))))))
               ((= dc dp)
                (setf p (if (< dp p) dp p)))
               (t
                (setf p (if (= dp 0) 0 (min dp (1- p))))))                                 
             (change-class directive 'f-directive
                           :k k
                           :e dp
                           :precision p
                           :parameters nil))
            (t
             (cond ((= dp 0)
                    (if (<= (abs exp) p)
                        (setf dp 1 p (1- p))
                        (setf dp 1 p 0)))
                   ((>= (abs dp) p)
                    (setf dp 0 p 0))
                   (t
                    (setf dp 1 p (1- p))))
             (let* ((vv (round v)))
               (if (< 0 vv 10)
                   (prin1-to-string vv)
                   (change-class directive 'e-directive
                                 :argument vv
                                 :precision p
                                 :parameters nil))))))))))
