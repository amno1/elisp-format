(in-package #:elisp-format)

(defclass elisp-directive (invistra::directive)
  ((argument
    :initarg :argument :accessor directive-argument)
   (consume-argument
    :initarg :consume-argument :initform nil :accessor consume-argument-p)
   (width
    :initarg :width :type integer :initform 0 :accessor argument-width)
   (precision
    :initarg :precision :type integer :accessor argument-precision)
   (prefix
    :initarg :prefix :initform nil :accessor argument-prefix)
   (padding
    :initarg :padding :type integer :initform 0 :accessor argument-padding)
   (padchar
    :initarg :padchar :type (or null character) :initform #\Space :accessor argument-padchar)
   (sign-char
    :initarg :sign-char :type (or null character) :initform nil :accessor sign-char)
   (padright
    :initarg :padright :type boolean :initform nil :accessor pad-right-p)
   (print-sign
    :initarg :print-sign :type boolean :initform nil :accessor print-sign-p)
   (pretty
    :initarg :pretty-print :initform nil :type (or null boolean) :accessor pretty-print)
   (restrict-width
    :initarg :restrict-width :type boolean :initform nil :accessor restrict-width)))

(defun make-parameter (string position)
  (if (find (aref string position) "123456789")
      (parse-integer string :start position :junk-allowed t)
      (values (aref string position) (1+ position))))

(defmethod parse-directive (control-string start)
  (let* ((end (length control-string))
         (last (array-last control-string))
         (directive (new 'elisp-directive
                         :control-string control-string
                         :directive-character last))
         stack token)
    (while (< start end)
           (multiple-value-bind (token pos)
               (make-parameter control-string start)
             (cond
               ((eql token #\$)
                (if (integerp (car stack))
                    (invistra::go-to-argument (1- (pop stack)) t)
                    (error "Invalid format operation '$', ~a" control-string)))
               ((and (eql token #\#))
                (setf (argument-prefix directive) t))
               ((eql token #\+)
                (unless (or (eql last #\s) (eql last #\c))
                  (setf (print-sign-p directive) t
                        (sign-char directive) #\+)))
               ((eql token #\-)
                (setf (slot-value directive 'padright) t))
               ((eql token #\Space)
                ;; + has precedence over space in elisp
                (unless (eql (sign-char directive) #\+)
                  (setf (print-sign-p directive) t
                        (sign-char directive) #\ )))
               ((and (eql token #\0) (char/= last #\c))
                (setf (argument-padchar directive) #\0))
               ((eql token #\.)
                (if (or (null stack) (integerp (car stack)))
                    (setf (argument-precision directive)
                          (cond
                            ((digit-char-p (char control-string pos))
                             (multiple-value-bind (precision p)
                                 (parse-integer control-string :start pos :junk-allowed t)
                               (setf pos p)
                               precision))
                            ((eql last #\c) 1)
                            (t 0)))
                    (error "Invalid format operation '.', ~a" control-string)))
               ((and (characterp token) (alpha-char-p token))
                (setf end pos))
               (t
                (push token stack)))
             (setf start pos)))

    (while stack
      (setf token (pop stack))
      (cond
        ((integerp token)
         (setf (argument-width directive) token
               (restrict-width directive) t))))
    
    (unless (slot-boundp directive 'precision)
      (setf (slot-value directive 'precision)
            (cond
              ((eql last #\c) 1)
              ((find last "efg") 6)
              (t nil))))
    
    (unless (eql last #\%)
      (setf (slot-value directive 'argument) (invistra::pop-argument)))
    (values directive end)))

(defmethod parse-control-string-fragment ((client elisp-client) control-string start)
  (let ((directive-position (position #\% control-string :start start)))
    (cond ((null directive-position)
           ;; No format char was found.  The rest of the control string
           ;; is just a string to be printed.
           (values (subseq control-string start)
                   (length control-string)))
          ((> directive-position start)
           ;; A format char was found, but it is not in the
           ;; start position.  A prefix of the control
           ;; string is therefore a string to be
           ;; printed.
           (values (subseq control-string start directive-position)
                   directive-position))
          (t
           ;; We found a tilde in the start position, so we have
           ;; a directive.
           (parse-directive control-string (1+ start))))))

;;; elisp-directive.lisp ends here
