(defconstant +SBCL-COMPRESSION+ t)

(require :sb-posix)
(require :cl-diagram)

;; lib 配下の *.stencil ファイルを検索して自動登録する
(let ((diagram:*include-paths* (list "../lib/")))
  (dolist (pathname (directory "../lib/*.stencil"))
	(let ((kwd (intern (string-upcase (pathname-name pathname)) 'keyword)))
	  (format t "loading ~A component...~%" kwd)
	  (diagram:load-stencil kwd))))


;; application entry ------------------------------------------
(defun application-entry ()
  (setf sb-impl::*default-external-format* :utf-8)
  (setf sb-alien::*default-c-string-external-format* :utf-8)
  (let ((self (car *posix-argv*))
		(args (cdr *posix-argv*)))
	(cl-diagram::cl-diagram-main self args)))
		

;; load application packages ---------------------------------
(sb-ext:save-lisp-and-die +OUTPUT-FILENAME+
						  :toplevel #'application-entry
						  :compression +SBCL-COMPRESSION+
						  :executable t
						  :save-runtime-options t)
