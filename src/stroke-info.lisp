#|
#|ASD|#				(:file "stroke-info"               :depends-on ("cl-diagram"
#|ASD|#																"colormap"))
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
;; class stroke-info
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:stroke-info
 |#
(defclass stroke-info ()
  ((color		:initform nil :initarg :color)			; (or keyword string)
   (width		:initform nil :initarg :width)			; number
   (opacity		:initform nil :initarg :opacity)		; number
   (linecap		:initform nil :initarg :linecap)		; (or nil keyword)
   (linejoin	:initform nil :initarg :linejoin)		; (or nil keyword)
   (miterlimit	:initform nil :initarg :miterlimit)		; number
   (dasharray	:initform nil :initarg :dasharray)		; list
   (dashoffset	:initform nil :initarg :dashoffset)))	; number

(defmethod initialize-instance :after ((stroke stroke-info) &rest initargs)
  (declare (ignore initargs))
  stroke)


(defmethod check ((ent stroke-info) canvas dict)
  (declare (ignore canvas dict))
  (with-slots (color width opacity linecap linejoin
					 miterlimit dasharray dashoffset) ent
	(check-member color      :nullable t :types (or string keyword))
	(when color
	  (setf color (colormap-fix color)))
	(check-member width      :nullable t :types number)
	(check-member opacity    :nullable t :types number)
	(check-member linecap    :nullable t :types keyword)
	(check-member linejoin   :nullable t :types keyword)
	(check-member miterlimit :nullable t :types number)
	(check-member dasharray  :nullable t :types list)
	(check-member dashoffset :nullable t :types number)
	(when linecap
	  (check-keywords linecap :butt :round :square))
	(when linejoin
	  (check-keywords linejoin :miter :round :bevel)))
  t)

(defmethod to-property-strings ((stroke stroke-info))
  (let ((color      (slot-value stroke      'color))
		(width      (slot-value stroke      'width))
		(opacity    (slot-value stroke    'opacity))
		(linecap    (slot-value stroke    'linecap))
		(linejoin   (slot-value stroke   'linejoin))
		(miterlimit (slot-value stroke 'miterlimit))
		(dasharr    (slot-value stroke  'dasharray))
		(dashoffset (slot-value stroke 'dashoffset)))
	(when color      (setf color      (format-string "stroke='" color "' ")))
	(when width	     (setf width      (format-string "stroke-width='" width "' ")))
	(when opacity    (setf opacity    (format-string "stroke-opacity='" opacity "' ")))
	(when linecap    (setf linecap    (format-string "stroke-linecap='" linecap "' ")))
	(when linejoin   (setf linejoin   (format-string "stroke-linejoin='" linejoin "' ")))
	(when miterlimit (setf miterlimit (format-string "stroke-miterlimit='" miterlimit "' ")))
	(when dasharr    (setf dasharr    (format nil "stroke-dasharray='~{~A ~}' " dasharr)))
	(when dashoffset (setf dashoffset (format-string "stroke-dashoffset='" dashoffset "' ")))
	(concatenate 'string color width opacity linecap linejoin miterlimit dasharr dashoffset)))

(defmethod to-style-strings ((stroke stroke-info))
  (let ((color      (slot-value stroke      'color))
		(width      (slot-value stroke      'width))
		(opacity    (slot-value stroke    'opacity))
		(linecap    (slot-value stroke    'linecap))
		(linejoin   (slot-value stroke   'linejoin))
		(miterlimit (slot-value stroke 'miterlimit))
		(dasharr    (slot-value stroke  'dasharray))
		(dashoffset (slot-value stroke 'dashoffset)))
	(when color      (setf color      (format-string "stroke: " color "; ")))
	(when width	     (setf width      (format-string "stroke-width: " width "; ")))
	(when opacity    (setf opacity    (format-string "stroke-opacity: " opacity "; ")))
	(when linecap    (setf linecap    (format-string "stroke-linecap: " linecap "; ")))
	(when linejoin   (setf linejoin   (format-string "stroke-linejoin: " linejoin "; ")))
	(when miterlimit (setf miterlimit (format-string "stroke-miterlimit: " miterlimit "; ")))
	(when dasharr    (setf dasharr    (format nil "stroke-dasharray: ~{~A ~}; " dasharr)))
	(when dashoffset (setf dashoffset (format-string "stroke-dashoffset: " dashoffset "; ")))
	(concatenate 'string color width opacity linecap linejoin miterlimit dasharr dashoffset)))


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
		  (destructuring-bind (&key (color      nil      color-p)
									(width      nil      width-p)
									(opacity    nil    opacity-p)
									(linecap    nil    linecap-p)
									(linejoin   nil   linejoin-p)
									(miterlimit nil miterlimit-p)
									(dasharray  nil  dasharray-p)
									(dashoffset nil dashoffset-p) base) params
			(let ((base (or base *default-stroke*)))
			  (labels ((fixval (val-p val slot-sym default)
						 (if val-p
							 val
							 (if base
								 (slot-value base slot-sym) default))))
				(make-instance 'stroke-info
							   :color      (fixval color-p      color      'color   :black)
							   :width      (fixval width-p      width      'width        1)
							   :opacity    (fixval opacity-p    opacity    'opacity    nil)
							   :linecap    (fixval linecap-p    linecap    'linecap    nil)
							   :linejoin   (fixval linejoin-p   linejoin   'linejoin   nil)
							   :miterlimit (fixval miterlimit-p miterlimit 'miterlimit nil)
							   :dasharray  (fixval dasharray-p  dasharray  'dasharray  nil)
							   :dashoffset (fixval dashoffset-p dashoffset 'dashoffset nil))))))))



(setf *default-stroke* (make-stroke :color   :black
									:width        1
									:opacity    nil
									:linecap    nil
									:linejoin   nil
									:miterlimit nil
									:dasharray  nil
									:dashoffset nil))

;;(to-property-strings (make-stroke))
;;(to-property-strings (make-stroke :red))
;;(to-property-strings (make-stroke 3))
;;(to-property-strings (make-stroke :color :red :width 4 :opacity 0.2))
;;(to-property-strings (make-stroke '(:color :red :width 4 :opacity 0.2)))

