#|
#|ASD|#				(:file "stencil"                   :depends-on ("cl-diagram"
#|ASD|#																"pathutil"))
#|EXPORT|#				;stencil.lisp
 |#


(in-package :cl-diagram)

;;#|
;;#|EXPORT|#				:*include-paths*
;;#|EXPORT|#				:*stencil-suffix*
;;#|EXPORT|#				:reset-stencil-load-cache
;;#|EXPORT|#				:load-stencil
;; |#
;;(defparameter *include-paths*  nil)
;;(defparameter *stencil-suffix* "stencil")
;;
;;(let ((include-cache nil)
;;	  (warn-list     '(:foo :bar)))    ;;ToDo : temporary...
;;;  (defun get-stencil-load-cache ()
;;;	  include-cache)
;;  (defun reset-stencil-load-cache ()
;;	  (setf include-cache nil))
;;  (defun load-stencil (stencil-name)
;;	(chk-type stencil-name keyword)
;;	(when (find stencil-name warn-list :test #'eq)
;;	  (throw-caution "No more need (load-stencil :~A)." stencil-name)
;;	  (return-from load-stencil))
;;	(unless include-cache
;;	  (setf include-cache (make-hash-table :test 'eq)))
;;	(let ((file-name (make-pathname :type *stencil-suffix*
;;									:name (string-downcase
;;										   (symbol-name stencil-name)))))
;;	  (labels ((find-stencil (lst)
;;				 (when lst
;;				   (let ((name (merge-pathnames file-name (car lst))))
;;					 ;;(format t "searching ~A ...~%" name)
;;					 (if (path/is-existing-file name)
;;						 name
;;						 (find-stencil (cdr lst)))))))
;;		(let* ((found (or (find-stencil *include-paths*)
;;						  (find-stencil `(,(path/get-current-directory)))))
;;			   (old-date (gethash stencil-name include-cache))
;;			   (new-date (and found (path/get-time found))))
;;		  (unless found
;;			(throw-exception "stencil named '~A' not found." stencil-name))
;;		  (when (or (null old-date) (string< old-date new-date))
;;			;;(format t "loading ~A ...~%" found)
;;			(load found)
;;			(setf (gethash stencil-name include-cache) new-date)))))))


