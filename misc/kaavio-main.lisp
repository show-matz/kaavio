(in-package :kaavio)

(defun kaavio-main (args)
  (handler-bind ((condition
                  (lambda (c)
                    (let ((is-warn (typep c 'warning)))
                      (format *error-output*
                              "~A : ~A~%" (if is-warn :WARNING :ERROR)
                              (with-output-to-string (stream)
                                (let ((*print-escape* nil))
                                  (print-object c stream))))
                      (if is-warn
                          (muffle-warning)
                          (return-from kaavio-main nil))))))
    (multiple-value-bind (paths args) (retrieve-include-paths args)
      (let ((kaavio:*include-paths* paths))
        (if (< 1 (length args))
            (kaavio::throw-exception "Invalid parameter count.")
            (labels ((read-whole (stream acc)
                       (let ((lst (read stream nil nil)))
                         (if (null lst)
                             (cons 'progn (nreverse acc))
                             (read-whole stream (cons lst acc)))))
                     (impl (stream)
                       (let ((*package* (find-package :kaavio-user)))
                         (write-string (eval (read-whole stream nil))))))
              (if (zerop (length args))
                  (impl *standard-input*)
                  (with-open-file (stream (car args) :direction :input)
                    (impl stream)))))))))
