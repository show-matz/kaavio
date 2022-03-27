(defconstant +SBCL-COMPRESSION+ t)

(require :sb-posix)
(require :cl-diagram)

;; @todo : lib 配下のファイルを検索して自動登録する
(load "../lib/grid.stencil")
(load "../lib/textbox.stencil")

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
						  :executable t)
