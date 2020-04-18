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
  ((color	:initform nil :initarg :color)		; (or keyword string)
   (opacity	:initform nil :initarg :opacity)	; number
   (rule	:initform nil :initarg :rule)))		; (or nil keyword)


(defmethod initialize-instance :after ((fill fill-info) &rest initargs)
  (declare (ignore initargs))
  fill)


(defmethod check ((fill fill-info) canvas dict)
  (declare (ignore canvas dict))
  (with-slots (color opacity rule) fill
	(check-member color   :nullable t :types (or string keyword))
	(check-member opacity :nullable t :types number)
	(check-member rule    :nullable t :types keyword)
	(when rule
	  (check-keywords rule :nonzero :evenodd)))
  t)

(defmethod to-property-strings ((fill fill-info))
  (let ((color   (slot-value fill 'color))
		(opacity (slot-value fill 'opacity))
		(rule    (slot-value fill 'rule)))
	(when color
	  (setf color (format-string "fill='" color "' ")))
	(when opacity
	  (setf opacity (format-string "fill-opacity='" opacity "' ")))
	(when rule
	  (setf rule (format-string "fill-rule='" rule "' ")))
	(concatenate 'string color opacity rule)))

(defmethod to-style-strings ((fill fill-info))
  (let ((color   (slot-value fill 'color))
		(opacity (slot-value fill 'opacity))
		(rule    (slot-value fill 'rule)))
	(when color
	  (setf color (format-string "fill: " color "; ")))
	(when opacity
	  (setf opacity (format-string "fill-opacity: " opacity "; ")))
	(when rule
	  (setf rule (format-string "fill-rule: " rule "; ")))
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
			  (labels ((fixval (val-p val slot-sym default)
						 (if val-p
							 val
							 (if base
								 (slot-value base slot-sym) default))))
				(make-instance 'fill-info
							   :color   (fixval color-p   color   'color  :none)
							   :opacity (fixval opacity-p opacity 'opacity  nil)
							   :rule    (fixval rule-p    rule    'rule     nil))))))))



(setf *default-fill* (make-fill :color :none
								:opacity nil
								:rule    nil))

;;(to-property-strings (make-fill))
;;(to-property-strings (make-fill :red))
;;(to-property-strings (make-fill 3))
;;(to-property-strings (make-fill :color :red :opacity 0.2))
;;(to-property-strings (make-fill '(:color :red :opacity 0.2)))

