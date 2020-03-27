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
   (linecap		;:type     (or nil keyword)
				:initform nil
				:initarg  :linecap
				:accessor stroke-linecap)
   (linejoin	;:type     (or nil keyword)
				:initform nil
				:initarg  :linejoin
				:accessor stroke-linejoin)
   (miterlimit	;:type     number
				:initform nil
				:initarg  :miterlimit
				:accessor stroke-miterlimit)
   (dasharray	:type     list
				:initform nil
				:initarg  :dasharray
				:accessor stroke-dasharray)
   (dashoffset	;:type     number
				:initform nil
				:initarg  :dashoffset
				:accessor stroke-dashoffset)))

(defmethod initialize-instance :after ((stroke stroke-info) &rest initargs)
  (declare (ignore initargs))
  stroke)


(defmethod check ((ent stroke-info) canvas dict)
  (declare (ignore canvas dict))
  (check-member (color      (stroke-color      ent)) :nullable t :types (or string keyword))
  (check-member (width      (stroke-width      ent)) :nullable t :types number)
  (check-member (opacity    (stroke-opacity    ent)) :nullable t :types number)
  (check-member (linecap    (stroke-linecap    ent)) :nullable t :types keyword)
  (check-member (linejoin   (stroke-linejoin   ent)) :nullable t :types keyword)
  (check-member (miterlimit (stroke-miterlimit ent)) :nullable t :types number)
  (check-member (dasharray  (stroke-dasharray  ent)) :nullable t :types list)
  (check-member (dashoffset (stroke-dashoffset ent)) :nullable t :types number)
  (when (stroke-linecap ent)
	(check-keywords (rule (stroke-linecap  ent)) :butt :round :square))
  (when (stroke-linejoin ent)
	(check-keywords (rule (stroke-linejoin ent)) :miter :round :bevel))
  t)

(defmethod to-property-strings ((stroke stroke-info))
  (let ((color      (stroke-color      stroke))
		(width      (stroke-width      stroke))
		(opacity    (stroke-opacity    stroke))
		(linecap    (stroke-linecap    stroke))
		(linejoin   (stroke-linejoin   stroke))
		(miterlimit (stroke-miterlimit stroke))
		(dasharr    (stroke-dasharray  stroke))
		(dashoffset (stroke-dashoffset stroke)))
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
  (let ((color      (stroke-color      stroke))
		(width      (stroke-width      stroke))
		(opacity    (stroke-opacity    stroke))
		(linecap    (stroke-linecap    stroke))
		(linejoin   (stroke-linejoin   stroke))
		(miterlimit (stroke-miterlimit stroke))
		(dasharr    (stroke-dasharray  stroke))
		(dashoffset (stroke-dashoffset stroke)))
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
			  (labels ((fixval (val-p val base-fnc default)
						 (if val-p val (if base (funcall base-fnc base) default))))
				(make-instance 'stroke-info
							   :color      (fixval color-p      color      #'stroke-color   :black)
							   :width      (fixval width-p      width      #'stroke-width        1)
							   :opacity    (fixval opacity-p    opacity    #'stroke-opacity    nil)
							   :linecap    (fixval linecap-p    linecap    #'stroke-linecap    nil)
							   :linejoin   (fixval linejoin-p   linejoin   #'stroke-linejoin   nil)
							   :miterlimit (fixval miterlimit-p miterlimit #'stroke-miterlimit nil)
							   :dasharray  (fixval dasharray-p  dasharray  #'stroke-dasharray  nil)
							   :dashoffset (fixval dashoffset-p dashoffset #'stroke-dashoffset nil))))))))



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

