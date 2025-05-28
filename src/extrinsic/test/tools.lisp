(in-package #:elisp-format-extrinsic-test)

(eval-when (:compile-toplevel)
  (defun gen-tests (tests)
    (loop while tests
          with result
          with => = '=>
          with fun = (pop tests)
          do
             (let ((args nil))
               (loop while (not (eq (car tests) =>))
                     do (push (pop tests) args))
               (pop tests)
               (push
                `(is equal (,fun ,@(nreverse args)) ,(pop tests))
                result))
          finally
             (return (nreverse result)))))

(defmacro def-test-group (name tests)
  (let ((tests (gen-tests (cdr tests))))
    `(parachute:define-test ,name
       ,@tests)))

(provide 'examples-to-tests)
