(require :cl-diagram)
(require :jp)
(require :pathnames)

(in-package :cl-diagram)

(labels ((load-whole-file (pathname)
		   (with-open-file (stream pathname :direction :input
											:element-type 'unsigned-byte)
			 (let* ((filesize (file-length stream))
					(buf      (make-array filesize :initial-element nil)))
			   (read-sequence buf stream)
			   buf))))

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
	  ;; ToDo : 環境によってはフルパスを取れないのでこれではダメっぽい
	  (let* ((dir-list (append (pathname-directory self) (list "lib")))
			 (lib-path (make-pathname :directory dir-list
									  :name nil :type nil :defaults self)))
		(setf diagram:*include-paths* (list lib-path)))

	  (if (/= 1 (length args))
		  (diagram::throw-exception "Invalid parameter count.")
		  (destructuring-bind (ifile) args
			(let ((in-file  (merge-pathnames ifile (path:get-current-directory))))
			  ;; check in-file existence.
			  (unless (path:is-existing-file in-file)
				(diagram::throw-exception "Input file '~A' is not exist." in-file))
			  ;; check encoding parameters.
			  (let* ((buf      (load-whole-file in-file))
					 (encoding (jp:guess buf)))
				(setf buf (concatenate 'string "(progn " (jp:decode buf encoding) ")"))
				(let ((*package* (find-package :diagram-user)))
				  (write-string (eval (read-from-string buf)))))))))))
