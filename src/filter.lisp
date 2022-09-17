#|
#|ASD|#				(:file "filter"                    :depends-on ("kaavio"
#|ASD|#																"writer"))
#|EXPORT|#				;filter.lisp
 |#

(in-package :kaavio)


#|
#|EXPORT|#				:*default-line-filter*
#|EXPORT|#				:*default-shape-filter*
 |#
(defparameter *default-line-filter*  nil)
(defparameter *default-shape-filter* nil)


;;------------------------------------------------------------------------------
;;
;; abstract class filter
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:filter
 |#
(defclass filter ()
  ((id		:initform nil :initarg :id)))	; keyword


#|
#|EXPORT|#				:write-filter
 |#
(defgeneric write-filter (filter writer))


#|
#|EXPORT|#				:with-line-filter
#|EXPORT|#				:with-shape-filter
 |#
(defmacro with-line-filter ((filter) &rest body)
  `(let ((*default-line-filter* ,filter))
	 ,@body))
(defmacro with-shape-filter ((filter) &rest body)
  `(let ((*default-shape-filter* ,filter))
	 ,@body))

