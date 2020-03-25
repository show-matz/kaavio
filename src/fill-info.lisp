#|
#|ASD|#				(:file "fill-info"                 :depends-on ("cl-diagram"))
#|EXPORT|#				;fill-info.lisp
 |#


(in-package :cl-diagram)

;; default parameter for fill
#|
#|EXPORT|#				:*default-fill*
 |#
(defparameter *default-fill* nil)

;;------------------------------------------------------------------------------
;;
;; fill-info
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:fill-info
 |#
(defclass fill-info ()
  ((color	;:type     (or keyword string)
			:initform nil
			:initarg  :color
			:accessor fill-color)
   (opacity	;:type     number
			:initform nil
			:initarg  :opacity
			:accessor fill-opacity)
   (rule	;:type     (or nil keyword)
			:initform nil
			:initarg  :rule
			:accessor fill-rule)))


(defmethod initialize-instance :after ((fill fill-info) &rest initargs)
  (declare (ignore initargs))
  fill)


(defmethod check ((fill fill-info) canvas dict)
  (declare (ignore canvas dict))
  (check-member (color   (fill-color   fill)) :nullable t :types (or string keyword))
  (check-member (opacity (fill-opacity fill)) :nullable t :types number)
  (check-member (rule    (fill-rule    fill)) :nullable t :types keyword)
  (when (fill-rule fill)
	(check-keywords (rule (fill-rule fill)) :nonzero :evenodd))
  t)

(defmethod to-property-strings ((fill fill-info))
  (let ((color   (fill-color   fill))
		(opacity (fill-opacity fill))
		(rule    (fill-rule    fill)))
	(when color
	  (setf color (format-string "fill='" color "' ")))
	(when opacity
	  (setf opacity (format-string "fill-opacity='" opacity "' ")))
	(when rule
	  (setf rule (format-string "fill-rule='" rule "' ")))
	(concatenate 'string color opacity rule)))


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
		  (destructuring-bind (&key (color   nil   color-p)
									(opacity nil opacity-p)
									(rule    nil    rule-p) base) params
			(let ((base (or base *default-fill*)))
			  (labels ((fixval (val-p val base-fnc default)
						 (if val-p val (if base (funcall base-fnc base) default))))
				(make-instance 'fill-info
							   :color   (fixval color-p   color   #'fill-color  :none)
							   :opacity (fixval opacity-p opacity #'fill-opacity  nil)
							   :rule    (fixval rule-p    rule    #'fill-rule     nil))))))))



(setf *default-fill* (make-fill :color :none
								:opacity nil
								:rule    nil))

;;(to-property-strings (make-fill))
;;(to-property-strings (make-fill :red))
;;(to-property-strings (make-fill 3))
;;(to-property-strings (make-fill :color :red :opacity 0.2))
;;(to-property-strings (make-fill '(:color :red :opacity 0.2)))

