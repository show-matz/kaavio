#|
#|ASD|#                (:file "filter"                    :depends-on ("kaavio"
#|ASD|#                                                                "writer"))
#|EXPORT|#                ;filter.lisp
 |#

(in-package :kaavio)


#|
#|EXPORT|#                :*default-filter*
 |#
(defparameter *default-filter* nil)


;;------------------------------------------------------------------------------
;;
;; abstract class filter
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#                :filter
 |#
(defclass filter ()
  ((id  :initform nil :initarg :id)))  ; keyword


#|
#|EXPORT|#                :write-filter
 |#
(defgeneric write-filter (filter writer))

