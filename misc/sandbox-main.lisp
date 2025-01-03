(require :kaavio)

(in-package :kaavio)

(defun read-whole (stream &optional acc)
  (let ((lst (read stream nil nil)))
    (if (null lst)
        (cons 'progn (nreverse acc))
        (read-whole stream (cons lst acc)))))

(defun eval-kaavio-data (stream)
  (handler-bind ((condition
                  (lambda (c)
                    (let* ((is-warn (typep c 'warning))
                           (msg (format nil "~A : ~A~%" (if is-warn :WARNING :ERROR)
                                        (with-output-to-string (stream)
                                          (let ((*print-escape* nil))
                                            (print-object c stream))))))
                      (if is-warn
                          (muffle-warning)
                          (return-from eval-kaavio-data msg))))))
    (let* ((input  (read-whole stream))
           (output (eval input)))
      (values output input))))

(defun generate-html (svg data stream)
  (format stream "<html>~%")
  (format stream "<head>~%")
  (format stream "  <meta charset='UTF-8'>~%")
  (format stream "  <meta http-equiv='refresh' content='2; URL='>~%")
  (format stream "  <title>kaavio sandbox</title>~%")
  (format stream "</head>~%")
  (format stream "<body>~%")
  (write-string svg stream)
  (format stream "<pre style=\"font-family: 'Courier New', monospace, serif\">~%")
  (print data stream)
  (format stream "</pre>~%")
  (format stream "</body>~%")
  (format stream "</html>~%"))

(defun create-skelton-file (file-name)
  (with-open-file (stream file-name :direction :output)
    (format stream "(diagram (400 300)~%")
    (format stream "  (grid)~%")
    (format stream "  ;; write code here...~%")
    (format stream ")~%")))
    

(defun sandbox-main (args)
  (multiple-value-bind (paths args) (retrieve-include-paths args)
    (let ((kaavio:*include-paths* paths))
      (if (< 2 (length args))
          (format *error-output* "ERROR : Invalid parameter count.~%")
          (let ((file-time 0)
                (pkg       (find-package :kaavio-user))
                (in-file   (or (first  args) "./sandbox.lisp"))
                (out-file  (or (second args) "./sandbox.html")))
            (unless (kaavio::path/is-exists in-file)
              (create-skelton-file in-file))
            (format t "IN  : ~A~%"  in-file)
            (format t "OUT : ~A~%" out-file)
            (handler-bind ((sb-sys:interactive-interrupt
                            (lambda (c)
                              (declare (ignore c))
                              (return-from sandbox-main nil))))
              (loop
                 (unless (kaavio::path/is-exists in-file)
                   (return-from sandbox-main nil))
                 (let ((new-time (kaavio::path/get-time in-file :value)))
                   (when (< file-time new-time)
                     (with-open-file (in in-file :direction :input)
                       (with-open-file (out out-file :direction :output :if-exists :supersede)
                         (let ((*package* pkg))
                           (multiple-value-bind (output input) (eval-kaavio-data in)
                             (generate-html output input out)))))
                     (setf file-time new-time)))
                 (sleep 0.1))))))))

