#|
#|ASD|#				(:file "filter"                    :depends-on ("cl-diagram"
#|ASD|#																"writer"))
#|EXPORT|#				;filter.lisp
 |#

(in-package :cl-diagram)


#|
#|EXPORT|#				:*default-filter*
#|EXPORT|#				:*default-text-filter*
#|EXPORT|#				:*default-line-filter*
#|EXPORT|#				:*default-shape-filter*
 |#
(defparameter *default-filter*       nil)
(defparameter *default-text-filter*  nil)
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

