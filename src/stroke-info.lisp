#|
#|ASD|#				(:file "stroke-info"               :depends-on ("cl-diagram"))
#|EXPORT|#				;stroke-info.lisp
 |#


(in-package :cl-diagram)

;; default parameter for stroke-info
#|
#|EXPORT|#				:*default-stroke*
 |#
(defparameter *default-stroke* nil)

;;------------------------------------------------------------------------------
;;
;; stroke-info
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:stroke-info
 |#
(defclass stroke-info ()
  ((color		;:type     (or keyword string)
				:initform nil
				:initarg  :color
				:accessor stroke-color)
   (width		;:type     number
				:initform nil
				:initarg  :width
				:accessor stroke-width)
   (opacity		;:type     number
				:initform nil
				:initarg  :opacity
				:accessor stroke-opacity)
   (dasharray	:type     list
				:initform nil
				:initarg  :dasharray
				:accessor stroke-dasharray)))

(defmethod initialize-instance :after ((stroke stroke-info) &rest initargs)
  (declare (ignore initargs))
  stroke)


(defmethod check ((ent stroke-info) canvas dict)
  (declare (ignore canvas dict))
  (check-member (color     (stroke-color     ent)) :nullable t :types (or string keyword))
  (check-member (width     (stroke-width     ent)) :nullable t :types number)
  (check-member (opacity   (stroke-opacity   ent)) :nullable t :types number)
  (check-member (dasharray (stroke-dasharray ent)) :nullable t :types list)
  t)

(defmethod to-property-strings ((stroke stroke-info))
  (let ((color   (stroke-color     stroke))
		(width   (stroke-width     stroke))
		(opacity (stroke-opacity   stroke))
		(dasharr (stroke-dasharray stroke)))
	(when color   (setf color   (format-string "stroke='" color "' ")))
	(when width	  (setf width   (format-string "stroke-width='" width "' ")))
	(when opacity (setf opacity (format-string "stroke-opacity='" opacity "' ")))
	(when dasharr (setf dasharr (format nil "stroke-dasharray='~{~A ~}' " dasharr)))
	(concatenate 'string color width opacity dasharr)))

(defmethod to-style-strings ((stroke stroke-info))
  (let ((color   (stroke-color     stroke))
		(width   (stroke-width     stroke))
		(opacity (stroke-opacity   stroke))
		(dasharr (stroke-dasharray stroke)))
	(when color   (setf color   (format-string "stroke: " color "; ")))
	(when width	  (setf width   (format-string "stroke-width: " width "; ")))
	(when opacity (setf opacity (format-string "stroke-opacity: " opacity "; ")))
	(when dasharr (setf dasharr (format nil "stroke-dasharray: ~{~A ~}; " dasharr)))
	(concatenate 'string color width opacity dasharr)))


#|
#|EXPORT|#				:make-stroke
 |#
(defun make-stroke (&rest params)
  (if (= 1 (length params))
	  (let ((param (car params)))
		(cond
		  ((typep param 'stroke-info) param)
		  ((numberp param) (make-stroke :width param))
		  ((listp   param) (apply #'make-stroke param))
		  (t               (make-stroke :color param))))
	  (if (and (null params) *default-stroke*)
		  *default-stroke*
		  (destructuring-bind (&key (color     nil     color-p)
									(width     nil     width-p)
									(opacity   nil   opacity-p)
									(dasharray nil dasharray-p) base) params
			(let ((base (or base *default-stroke*)))
			  (labels ((fixval (val-p val base-fnc default)
						 (if val-p val (if base (funcall base-fnc base) default))))
				(make-instance 'stroke-info
							   :color     (fixval color-p     color     #'stroke-color  :black)
							   :width     (fixval width-p     width     #'stroke-width       1)
							   :opacity   (fixval opacity-p   opacity   #'stroke-opacity   nil)
							   :dasharray (fixval dasharray-p dasharray #'stroke-dasharray nil))))))))



(setf *default-stroke* (make-stroke :color  :black
									:width       1
									:opacity   nil
									:dasharray nil))

;;(to-property-strings (make-stroke))
;;(to-property-strings (make-stroke :red))
;;(to-property-strings (make-stroke 3))
;;(to-property-strings (make-stroke :color :red :width 4 :opacity 0.2))
;;(to-property-strings (make-stroke '(:color :red :width 4 :opacity 0.2)))

