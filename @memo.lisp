;;(load "./@load.lisp")
(require :kaavio)
(load "./misc/kaavio-main.lisp")

(let ((path (if (member :linux *features*)
				"~/sandbox.github/kaavio.github/lib/"
				(merge-pathnames "kaavio.github/lib/"
								 (sb-ext:posix-getenv "CVS_SANDBOX")))))
  (setf kaavio:*include-paths* (list path)))

;(path:set-current-directory (if (member :linux *features*)
;										 "./sample"
;										 "C:/sandbox/kaavio/sample/"))
;(path:get-current-directory)

;;usage (update-svg-files "sample/" :all t)
(defun update-svg-files (rel-path &key all)
  (let* ((org-path (path:get-current-directory))
		 (new-path (merge-pathnames rel-path org-path))
		 (pred (if all
				   (constantly t)
				   (lambda (kaavio-file svg-file)
					 (or (not (path:is-existing-file svg-file))
						 (let ((time1 (path:get-time kaavio-file))
							   (time2 (path:get-time     svg-file)))
						   (string< time2 time1)))))))
	(path:set-current-directory new-path)
	(dolist (file (path:list-directory new-path))
	  (when (string= "kaavio" (pathname-type file))
		(let ((outfile (make-pathname :type "svg" :defaults file)))
		  (when (funcall pred file outfile)
			(format t "~A~%" file)
			(kaavio::kaavio-main "./" `(,file ,outfile))))))
	(path:set-current-directory org-path)))
