;;(load "./@load.lisp")
(require :cl-diagram)
(load "./misc/cl-apps-main.lisp")

(let ((path (if (member :linux *features*)
				"~/sandbox/cl-diagram/lib/"
				(merge-pathnames "cl-diagram/lib/"
								 (sb-ext:posix-getenv "CVS_SANDBOX")))))
  (setf diagram:*include-paths* (list path)))

;(path:set-current-directory (if (member :linux *features*)
;										 "./sample"
;										 "C:/sandbox/cl-diagram/sample/"))
;(path:get-current-directory)

;;usage (update-svg-files "sample/" :all t)
(defun update-svg-files (rel-path &key all)
  (let* ((org-path (path:get-current-directory))
		 (new-path (merge-pathnames rel-path org-path))
		 (pred (if all
				   (constantly t)
				   (lambda (diagram-file svg-file)
					 (or (not (path:is-existing-file svg-file))
						 (let ((time1 (path:get-time diagram-file))
							   (time2 (path:get-time     svg-file)))
						   (string< time2 time1)))))))
	(path:set-current-directory new-path)
	(dolist (file (path:list-directory new-path))
	  (when (string= "diagram" (pathname-type file))
		(let ((outfile (make-pathname :type "svg" :defaults file)))
		  (when (funcall pred file outfile)
			(format t "~A~%" file)
			(cl-apps-main `(,file :utf8 ,outfile :utf8))))))
	(path:set-current-directory org-path)))
