(defconstant +SBCL-COMPRESSION+ t)

(require :sb-posix)
(require :kaavio)

;; lib 配下の *.stencil ファイルを検索して自動登録する
(let ((kaavio:*include-paths* (list "../lib/")))
  (dolist (pathname (directory "../lib/*.stencil"))
	(let ((kwd (intern (string-upcase (pathname-name pathname)) 'keyword)))
	  (format t "loading ~A component...~%" kwd)
	  (kaavio:load-stencil kwd))))


;; application entry ------------------------------------------
(defun application-entry ()
  (setf sb-impl::*default-external-format* :utf-8)
  (setf sb-alien::*default-c-string-external-format* :utf-8)
  (let ((self (car *posix-argv*))
		(args (cdr *posix-argv*)))
	(cond
	  ((string= (car args) "--help")
	   (progn
		 (format t "usage : ~A --help~%" self)
		 (format t "usage : ~A --sandbox [-IPATH]... INFILE OUTFILE~%" self)
		 (format t "usage : ~A [-IPATH]... [INFILE]~%" self)))
	  ((string= (car args) "--sandbox")
	   (kaavio::sandbox-main (cdr args)))
	  (t
	   (kaavio::kaavio-main args)))))
		

;; load application packages ---------------------------------
(sb-ext:save-lisp-and-die +OUTPUT-FILENAME+
						  :toplevel #'application-entry
						  :compression +SBCL-COMPRESSION+
						  :executable t
						  :save-runtime-options t)
