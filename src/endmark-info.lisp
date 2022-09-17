#|
#|ASD|#				(:file "endmark-info"              :depends-on ("kaavio"
#|ASD|#																"constants"
#|ASD|#																"mathutil"
#|ASD|#																"point"
#|ASD|#																"canvas"
#|ASD|#																"dictionary"
#|ASD|#																"fill-info"
#|ASD|#																"stroke-info"
#|ASD|#																"writer"))
#|EXPORT|#				;endmark-info.lisp
 |#


(in-package :kaavio)


;;------------------------------------------------------------------------------
;;
;; internal functions
;;
;;------------------------------------------------------------------------------
(defun __draw-endmark-arrow (points size stroke fill writer)
  (declare (ignore fill))
  (symbol-macrolet ((ARROW_DEGREE1 210)
					(ARROW_DEGREE2 150))
	(let* ((pt1 (car points))
		   (pt2 (cdr points))
		   (x1 (* size (math/cos3 pt1 pt2 ARROW_DEGREE1)))
		   (y1 (* size (math/sin3 pt1 pt2 ARROW_DEGREE1)))
		   (x2 (* size (math/cos3 pt1 pt2 ARROW_DEGREE2)))
		   (y2 (* size (math/sin3 pt1 pt2 ARROW_DEGREE2))))
	  (writer-write writer
					"<polyline "
					"fill='none' "
					(when stroke
					  (to-property-strings stroke))
					"points='"	(+ (point-x pt2) x2) "," (+ (point-y pt2) y2) " "
								(point-x pt2)        "," (point-y pt2)        " "
								(+ (point-x pt2) x1) "," (+ (point-y pt2) y1) "' "
					"/>"))))

(defun __draw-endmark-triangle (points size stroke fill writer)
  (symbol-macrolet ((ARROW_DEGREE1 205)
					(ARROW_DEGREE2 155))
	(let* ((pt1 (car points))
		   (pt2 (cdr points))
		   (x1 (* size (math/cos3 pt1 pt2 ARROW_DEGREE1)))
		   (y1 (* size (math/sin3 pt1 pt2 ARROW_DEGREE1)))
		   (x2 (* size (math/cos3 pt1 pt2 ARROW_DEGREE2)))
		   (y2 (* size (math/sin3 pt1 pt2 ARROW_DEGREE2))))
	  (writer-write writer
					"<path "
					(when fill
					  (to-property-strings fill))
					(when stroke
					  (to-property-strings stroke))
					"d='M " (point-x pt2) " " (point-y pt2) " "
					   "l " x1            " " y1            " "
					   "l " (- x2 x1)     " " (- y2 y1)     " z' "
					"/>"))))

(defun __draw-endmark-diamond (points size stroke fill writer)
  (symbol-macrolet ((ARROW_DEGREE1 210)
					(ARROW_DEGREE2 150))
	(let* ((pt1 (car points))
		   (pt2 (cdr points))
		   (x1 (point-x pt2))
		   (y1 (point-y pt2))
		   (x2 (+ x1 (* size (math/cos3 pt1 pt2 ARROW_DEGREE1))))
		   (y2 (+ y1 (* size (math/sin3 pt1 pt2 ARROW_DEGREE1))))
		   (x4 (+ x1 (* size (math/cos3 pt1 pt2 ARROW_DEGREE2))))
		   (y4 (+ y1 (* size (math/sin3 pt1 pt2 ARROW_DEGREE2))))
		   (x3 (+ x2 (- x4 x1)))
		   (y3 (+ y2 (- y4 y1))))
	  (writer-write writer
					"<path "
					(when fill
					  (to-property-strings fill))
					(when stroke
					  (to-property-strings stroke))
					"d='M " x1 " " y1 " L " x2 " " y2 " "
					   "L " x3 " " y3 " L " x4 " " y4 " z' "
					"/>"))))

(defun __draw-endmark-circle (points size stroke fill writer)
  (let ((pt (cdr points)))
	(writer-write writer
				  "<circle "
				  "cx='" (point-x pt) "' "
				  "cy='" (point-y pt) "' "
				  "r='" (/ size 2) "' "
				  (when fill
					(to-property-strings fill))
				  (when stroke
					(to-property-strings stroke))
				  "/>")))

(defun __draw-endmark-rectangle (points size stroke fill writer)
  (let* ((pt (cdr points)))
	(writer-write writer
				  "<rect "
				  "x='" (- (point-x pt) (/ size 2)) "' "
				  "y='" (- (point-y pt) (/ size 2)) "' "
				  "width='" size "' "
				  "height='" size "' "
				  (when fill
					(to-property-strings fill))
				  (when stroke
					(to-property-strings stroke))
				  "/>")))


;;------------------------------------------------------------------------------
;;
;; class endmark-info
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:endmark-info
 |#
(defclass endmark-info ()
  ((type	:initform nil :initarg :type)		; (or keyword function)
												; :none|:arrow|:triangle|:diamond|:circle|:rect
   (size	:initform nil :initarg :size)		; (or keyword number)
												; :small|:midium|:large|:xlarge
   (fill	:initform nil :initarg :fill)		; (or nil fill-info)	; nil means same as stroke
   (stroke	:initform nil :initarg :stroke)))	; (or nil stroke-info)


(defmethod initialize-instance :after ((mark endmark-info) &rest initargs)
  (declare (ignore initargs))
  (with-slots (type size fill stroke) mark
	(setf type   (or type *default-endmark-type*))
	(setf size   (or size *default-endmark-size*))
	(setf fill   (make-fill   (or fill   *default-fill*)))
	(setf stroke (make-stroke (or stroke *default-stroke*))))
  mark)


(defmethod check ((mark endmark-info) canvas dict)
  (with-slots (type size fill stroke) mark
	(check-member type   :nullable nil :types (or keyword function))
	(check-member size   :nullable nil :types (or keyword number))
	(check-object fill   canvas dict :nullable t :class   fill-info)
	(check-object stroke canvas dict :nullable t :class stroke-info)
	(when (keywordp type)
	  (check-keywords type :arrow :triangle :diamond :circle :rect))
	(when (keywordp size)
	  (check-keywords size :small :midium :large :xlarge)))
  t)

(defun draw-endmark (mark points stroke writer)
  (with-slots (type size fill) mark
	(let* ((size (if (numberp size)
					 size
					 (ecase size
					   ((:small)  10.0)
					   ((:midium) 15.0)
					   ((:large)  20.0)
					   ((:xlarge) 30.0))))
		   (drawer (if (functionp type)
					   type
					   (ecase type
						 ((:arrow)    #'__draw-endmark-arrow)
						 ((:triangle) #'__draw-endmark-triangle)
						 ((:diamond)  #'__draw-endmark-diamond)
						 ((:circle)   #'__draw-endmark-circle)
						 ((:rect)     #'__draw-endmark-rectangle)))))
	  (funcall drawer points size
			   (or (slot-value mark 'stroke) stroke)    ;ToDo : m-stroke is always not nil...
			   fill writer))))
  

#|
#|EXPORT|#				:make-endmark
 |#
(defun make-endmark (&rest params)
  (if (= 1 (length params))
	  (let ((param (car params)))
		(cond
		  ((typep param 'endmark-info) param)
		  ((keywordp param) (make-endmark :type param))
		  ((numberp  param) (make-endmark :size param))
		  ((listp    param) (apply #'make-endmark param))
		  (t                (make-endmark :type param))))
	  (if (null params)
		  nil
		  (destructuring-bind (&key type size fill stroke) params
			(make-instance 'endmark-info
						   :type   type
						   :size   size
						   :fill   fill
						   :stroke stroke)))))

