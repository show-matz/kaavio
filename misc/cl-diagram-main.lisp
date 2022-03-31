(require :cl-diagram)

(in-package :cl-diagram)    ;;ToDo : :diagram-user でもいける？ → path/xxx の参照が問題になる

(labels ((read-whole-file (pathname)
		   (with-open-file (in pathname :direction :input)
			 (labels ((impl (acc)
						(let ((lst (read in nil nil)))
						  (if (null lst)
							  (cons 'progn (nreverse acc))
							  (impl (cons lst acc))))))
			   (impl nil)))))

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
			(let ((in-file  (merge-pathnames ifile (path/get-current-directory))))	;; ToDo : merge 不要？
			  ;; check in-file existence.
			  (unless (path/is-existing-file in-file)
				(diagram::throw-exception "Input file '~A' is not exist." in-file))
			  (let ((*package* (find-package :diagram-user)))
				(write-string (eval (read-whole-file in-file))))))))))
