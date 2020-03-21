#|
#|ASD|#				(:file "fill-info"                 :depends-on ("cl-diagram"
#|ASD|#																"constants"))
#|EXPORT|#				;fill-info.lisp
 |#


(in-package :cl-diagram)

;;------------------------------------------------------------------------------
;;
;; fill-info
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:fill-info
 |#
(defclass fill-info ()
  ((color ;:type     (or keyword string)
		  :initform nil
		  :initarg  :color
		  :accessor fill-color)
   (opacity ;:type     number
			:initform nil
			:initarg  :opacity
			:accessor fill-opacity)))


(defmethod initialize-instance :after ((fill fill-info) &rest initargs)
  (declare (ignore initargs))
  (with-slots (color opacity) fill
	(setf color   (or color   *default-fill-color*))
	(setf opacity (or opacity *default-fill-opacity*)))
  fill)


(defmethod check ((fill fill-info) canvas dict)
  (declare (ignore canvas dict))
  (check-member (color   (fill-color   fill)) :nullable t :types (or string keyword))
  (check-member (opacity (fill-opacity fill)) :nullable t :types number)
  t)

(defmethod to-property-strings ((fill fill-info))
  (let ((color   (fill-color     fill))
		(opacity (fill-opacity   fill)))
	(when color
	  (setf color (format-string "fill='" color "' ")))
	(when opacity
	  (setf opacity (format-string "fill-opacity='" opacity "' ")))
	(concatenate 'string color opacity)))


#|
#|EXPORT|#				:make-fill
 |#
(defun make-fill (&rest params)
  (if (= 1 (length params))
	  (let ((param (car params)))
		(cond
		  ((typep param 'fill-info) param)
		  ((listp param) (apply #'make-fill param))
		  (t             (make-fill :color param))))
	  (if (and (null params) *default-fill*)
		  *default-fill*
		  (destructuring-bind (&key color opacity base) params
			(let ((base (or base *default-fill*)))
			  (if (null base)
				  (make-instance 'fill-info
								 :color   color
								 :opacity opacity)
				  (make-instance 'fill-info
								 :color   (or color   (fill-color   base))
								 :opacity (or opacity (fill-opacity base)))))))))


;;(to-property-strings (make-fill))
;;(to-property-strings (make-fill :red))
;;(to-property-strings (make-fill 3))
;;(to-property-strings (make-fill :color :red :opacity 0.2))
;;(to-property-strings (make-fill '(:color :red :opacity 0.2)))

