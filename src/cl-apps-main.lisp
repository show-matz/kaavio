#|
#|ASD|#				(:file "cl-apps-main"              :depends-on ("cl-diagram"
#|ASD|#																"pathutil"
#|ASD|#																"create-svg"))
#|EXPORT|#				;cl-apps-main.lisp
 |#

(in-package :cl-diagram)


(labels ((fix-encoding (arg candidates)
		   (let ((kwd (intern (string-upcase arg) :keyword)))
			 (if (member kwd candidates)
				 kwd
				 (throw-exception "Invalid encoding '~A'." arg))))

		 (load-whole-file (pathname)
		   (with-open-file (stream pathname :direction :input
											:element-type 'unsigned-byte)
			 (let* ((filesize (file-length stream))
					(buf      (make-array filesize :initial-element nil)))
			   (read-sequence buf stream)
			   buf)))

		 (save-whole-file (pathname byte-buf)
		   (handler-case
			   (with-open-file (stream pathname :direction :output
												:if-exists :supersede
												:element-type 'unsigned-byte)
				 (write-sequence byte-buf stream))
			 (error ()
			   (throw-exception "Output file '~A' can't open." pathname)))
		   nil))

  ;;-------------------------------------------------------------------------------
  ;;
  ;; defun cl-apps-main
  ;;
  ;;-------------------------------------------------------------------------------
  #|
  #|EXPORT|#				:cl-apps-main
  |#
  (defun cl-apps-main (args)
	(handler-bind ((condition
					(lambda (c)
					  (let ((is-warn (typep c 'warning)))
						(format t "~A : ~A~%" (if is-warn :WARNING :ERROR)
											  (with-output-to-string (stream)
												(let ((*print-escape* nil))
												  (print-object c stream))))
						(if is-warn
							(muffle-warning)
							(return-from cl-apps-main nil))))))
	  (if (/= 4 (length args))
		  (throw-exception "Invalid parameter count.")
		  (destructuring-bind (in-file   input-encoding
							   out-file output-encoding) args
			;; check in-file existence.
			(unless (path/is-existing-file in-file)
			  (throw-exception "Input file '~A' is not exist." in-file))
			;; check encoding parameters.
			(let* ((candidates '(:guess :jis :euc-jp :sjis :utf8 :ascii))
				   (enc1 (fix-encoding  input-encoding candidates))
				   (enc2 (fix-encoding output-encoding (cdr candidates)))
				   (buf (load-whole-file in-file))
				   (encoding (if (eq enc1 :guess)
								 (jp:guess buf)
								 (jp:make-encoding enc1))))
			  (setf buf (concatenate 'string "(progn " (jp:decode buf encoding) ")"))
			  (let ((diagram:*default-output-encoding* enc2)
					(*package* (find-package :svg-user)))
				(setf buf (eval (read-from-string buf))))
			  (save-whole-file out-file (jp:encode buf enc2))))))))
