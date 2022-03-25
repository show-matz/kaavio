(defconstant +SBCL-COMPRESSION+ t)

(require :sb-posix)
(require :cl-diagram)

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
