#|
#|ASD|#				(:file "circle"                    :depends-on ("cl-diagram"
#|ASD|#																"constants"
#|ASD|#																"mathutil"
#|ASD|#																"canvas"
#|ASD|#																"point"
#|ASD|#																"shape"
#|ASD|#																"stroke-info"
#|ASD|#																"link-info"
#|ASD|#																"filter"
#|ASD|#																"writer"))
#|EXPORT|#				;circle.lisp
 |#


(in-package :cl-diagram)

;;------------------------------------------------------------------------------
;;
;; utility functions
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:circle-connect-point
 |#
(defun circle-connect-point (center radius type1 type2 arg)
  (declare (ignore type1))
  (let ((cx (point-x center))
		(cy (point-y center)))
	(if (eq type2 :center)
		(with-point (px py) arg
					(let ((x (* radius (math/cos4 px py cx cy)))
						  (y (* radius (math/sin4 px py cx cy))))
					  (make-point (- cx x) (- cy y) :absolute)))
		(let ((degree (ecase type2
						((:right)  (+   0 (* arg 30)))
						((:bottom) (-  90 (* arg 30)))
						((:left)   (- 180 (* arg 30)))
						((:top)    (+ 270 (* arg 30))))))
		  (when (< degree 0)
			(incf degree 360))
		  (let ((x (* radius (math/cos1 degree)))
				(y (* radius (math/sin1 degree))))
			(make-point (+ cx x) (+ cy y) :absolute))))))


;;------------------------------------------------------------------------------
;;
;; class circle
;;
;;------------------------------------------------------------------------------
(defclass circle (shape)
  ((center		:initform nil :initarg :center)		; point
   (radius		:initform   0 :initarg :radius)		; number
   (fill		:initform nil :initarg :fill)		; (or nil fill-info)
   (stroke		:initform nil :initarg :stroke)		; (or nil stroke-info)
   (filter		:initform nil :initarg :filter)))	; (or nil keyword)

(defmethod initialize-instance :after ((ent circle) &rest initargs)
  (declare (ignore initargs))
  (with-slots (fill stroke filter) ent
	(setf fill   (make-fill   (or fill   *default-fill*)))
	(setf stroke (make-stroke (or stroke *default-stroke*)))
	(setf filter (if (eq filter :none)
					 nil
					 (or filter *default-shape-filter*))))
  ent)

(defmethod check ((shp circle) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (center radius fill stroke filter) shp
	(check-member radius   :nullable nil :types number)
	(check-object fill     canvas dict :nullable t :class   fill-info)
	(check-object stroke   canvas dict :nullable t :class stroke-info)
	(check-member filter   :nullable   t :types keyword)
	(setf filter (if (eq filter :none) nil filter))
	(setf center (canvas-fix-point canvas center)))
  nil)

(defmethod shape-width ((shp circle))
  (* 2 (slot-value shp 'radius)))

(defmethod shape-height ((shp circle))
  (* 2 (slot-value shp 'radius)))

(defmethod shape-center ((shp circle))
  (slot-value shp 'center))


(defmethod shape-connect-point ((shp circle) type1 type2 arg)
  (circle-connect-point (shape-center shp)
						(slot-value shp 'radius) type1 type2 arg))
  
;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((shp circle)) ...)

;;MEMO : use impelementation of shape...
;;(defmethod entity-composition-p ((shp circle)) ...)
  
(defmethod draw-entity ((shp circle) writer)
  (with-slots (center radius fill stroke filter) shp
	(let ((id (and (not (entity-composition-p shp))
				   (slot-value shp 'id))))
	  (pre-draw shp writer)
	  (writer-write writer
					"<circle "
					(write-when id "id='" it "' ")
					"cx='" (point-x center) "' "
					"cy='" (point-y center) "' "
					"r='" radius "' "
					(when fill
					  (to-property-strings fill))
					(when stroke
					  (to-property-strings stroke))
					(write-when filter "filter='url(#" it ")' ")
					"/>")
	  (post-draw shp writer)))
  nil)


;;------------------------------------------------------------------------------
;;
;; macro circle
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:circle
 |#
(defmacro circle (center radius
				  &key fill stroke link layer id filter contents)
  (let ((code `(register-entity (make-instance 'diagram:circle
											   :center ,center :radius ,radius
											   :fill ,fill :stroke ,stroke :link ,link
											   :filter ,filter :layer ,layer :id ,id))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (diagram:shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))

