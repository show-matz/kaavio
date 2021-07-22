;(load #+sbcl  (concatenate 'string (posix-getenv "SBCL_HOME") "contrib\\asdf.fasl")
;      #+clisp "/cygdrive/d/user/lib/clisp/asdf.fas")
;
;(push #+sbcl  "D:/user/lib/sbcl/asd/"
;      #+clisp "/cygdrive/d/user/lib/clisp/asd/"
;      asdf:*central-registry*)


#+sbcl
(defconstant +SBCL-COMPRESSION+ nil)

#+sbcl (require :sb-posix)
(require :pathnames)

(let ((apps '(;(:CL-PREFAB   "cl-prefab"  :prefab  "MAIN")
			  (:CL-DIAGRAM  "cl-diagram" :diagram  "CL-DIAGRAM-MAIN"))))

  ;; load packages ----------------------------------------------
  (mapcar (lambda (info)
			(let ((pkg (first info)))
			  #+sbcl  (require pkg)
			  #+clisp (asdf:load-system pkg))) apps)

  ;; application entry ------------------------------------------
  (defun application-entry ()

;;	;; load .rc file
;;	(let ((rcfile (merge-pathnames ".clappsrc"
;;								   (path:get-as-directory-name 
;;									(sb-ext:posix-getenv "HOME")))))
;;	  ;;(format t "checking ~A...~%" rcfile)
;;	  (when (path:is-existing-file rcfile)
;;		;;(format t ".clappsrc file is found, now loading...~%")
;;		(unless (handler-case (progn (load rcfile) t)
;;				  (error () nil))
;;		  (format t "ERROR : fail to load ~A file.~%" rcfile)
;;		  #+clisp (ext:exit)
;;		  (return-from application-entry nil))))

	(let ((self #+clisp +OUTPUT-FILENAME+
				#+sbcl  (car *posix-argv*)
				#-(or clisp sbcl) (error "not yet implemented."))
		  (args #+clisp *ARGS*
				#+sbcl  (cdr *posix-argv*)
				#-(or clisp sbcl) (error "not yet implemented.")))
	  (let ((app (find-if (lambda (info)
							(string= (second info) (car args))) apps)))
		(if (null app)
			(format t "ERROR : application ~A is not registerd." (car args))
			(let ((fnc (symbol-function
						(find-symbol (fourth app)
									 (find-package (third app))))))
			  (funcall fnc self (cdr args))))))
	#+clisp (ext:exit)))
		

;; load application packages ---------------------------------
(let ((entry-point #'application-entry))
  #+sbcl
  (sb-ext:save-lisp-and-die +OUTPUT-FILENAME+
                            :toplevel entry-point
                            :compression +SBCL-COMPRESSION+
                            :executable t)
  #+clisp
  (ext:saveinitmem +OUTPUT-FILENAME+
                   :quiet t
                   :norc  t
                   :init-function entry-point
                   :executable t))

