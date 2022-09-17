#|
#|ASD|#				(:file "link-info"                 :depends-on ("kaavio"
#|ASD|#																"constants"
#|ASD|#																"writer"))
#|EXPORT|#				;link-info.lisp
 |#

(in-package :kaavio)

;;------------------------------------------------------------------------------
;;
;; link-info
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:link-info
 |#
(defclass link-info ()
  ((url		:initform nil :initarg :url)		; string
   (target	:initform nil :initarg :target)))	; keyword - :replace :self :parent :top :blank

(defmethod initialize-instance :after ((link link-info) &rest initargs)
  (declare (ignore initargs))
  (with-slots (target) link
	(setf target (or target *default-link-target*)))
  link)

(defmethod check ((ent link-info) canvas dict)
  (declare (ignore canvas dict))
  (with-slots (url target) ent
	(check-member url    :nullable nil :types string)
	(check-member target :nullable   t :types keyword)
	(when target
	  (check-keywords target :replace :self :parent :top :blank)))
  nil)

#|
#|EXPORT|#				:write-link-open
 |#
(defun write-link-open (link writer)
  (when link
	(with-slots (url target) link
	  (writer-write writer "<a xlink:href='" url "'"
							  (write-when target " target='_" it "'") ">"))
	(writer-incr-level writer)))

#|
#|EXPORT|#				:write-link-close
 |#
(defun write-link-close (link writer)
  (when link
	(writer-decr-level writer)
	(writer-write writer "</a>")))

#|
#|EXPORT|#				:make-link
 |#
(defun make-link (&rest params)
  (if (= 1 (length params))
	  (let ((param (car params)))
		(cond
		  ((typep param 'link-info) param)
		  ((listp param) (apply #'make-link param))
		  (t             (make-link :url param))))
	  (if (null params)
		  nil
		  (destructuring-bind (&key url target) params
			(make-instance 'link-info :url url :target target)))))


;;(make-link)
;;(make-link "http://www.google.co.jp/"))
;;(make-link :url "http://www.google.co.jp/" :target :top)
;;(make-link '(:url "http://www.google.co.jp/" :target :top))

