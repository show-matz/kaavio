(require :cl-diagram)
(require :jp)
(require :pathnames)

(in-package :cl-diagram)

(labels ((fix-encoding (arg candidates)
		   (let ((kwd (intern (string-upcase arg) :keyword)))
			 (if (member kwd candidates)
				 kwd
				 (diagram::throw-exception "Invalid encoding '~A'." arg))))

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
			   (diagram::throw-exception "Output file '~A' can't open." pathname)))
		   nil))

  (defun cl-diagram-main (self args)
	(handler-bind ((condition
					(lambda (c)
					  (let ((is-warn (typep c 'warning)))
						(format t "~A : ~A~%" (if is-warn :WARNING :ERROR)
											  (with-output-to-string (stream)
												(let ((*print-escape* nil))
												  (print-object c stream))))
						(if is-warn
							(muffle-warning)
							(return-from cl-diagram-main nil))))))

	  ;; 起動した実行可能ファイルと同じ場所にある lib ディレクトリ配下を include-path に指定する
	  (let ((lib-path (make-pathname :name nil :type nil :defaults self
									 :directory (append (pathname-directory self) (list "lib")))))
		(setf diagram:*include-paths* (list lib-path)))

	  (if (/= 4 (length args))
		  (diagram::throw-exception "Invalid parameter count.")
		  (destructuring-bind (ifile  input-encoding
							   ofile output-encoding) args
			(let ((in-file  (merge-pathnames ifile (path:get-current-directory)))
				  (out-file (merge-pathnames ofile (path:get-current-directory))))
			  ;; check in-file existence.
			  (unless (path:is-existing-file in-file)
				(diagram::throw-exception "Input file '~A' is not exist." in-file))
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
					  (*package* (find-package :diagram-user)))
				  (setf buf (eval (read-from-string buf))))
				(save-whole-file out-file (jp:encode buf enc2)))))))))
