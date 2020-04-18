#|
#|ASD|#				(:file "circle"                    :depends-on ("cl-diagram"
#|ASD|#																"constants"
#|ASD|#																"mathutil"
#|ASD|#																"canvas"
#|ASD|#																"point"
#|ASD|#																"shape"
#|ASD|#																"stroke-info"
#|ASD|#																"link-info"
#|ASD|#																"writer"))
#|EXPORT|#				;circle.lisp
 |#


(in-package :cl-diagram)

;-------------------------------------------------------------------------------
;
; utility functions
;
;-------------------------------------------------------------------------------
#|
#|EXPORT|#				:circle-connect-point
 |#
(defun circle-connect-point (cx cy radius type arg)
  (if (eq type :center)
	  (with-point (px py) arg
		(let ((x (* radius (math/cos4 px py cx cy)))
			  (y (* radius (math/sin4 px py cx cy))))
		  (make-point (- cx x) (- cy y))))
	  (let ((degree (ecase type
					  ((:right)  (+   0 (* arg 30)))
					  ((:bottom) (-  90 (* arg 30)))
					  ((:left)   (- 180 (* arg 30)))
					  ((:top)    (+ 270 (* arg 30))))))
		(when (< degree 0)
		  (incf degree 360))
		(let ((x (* radius (math/cos1 degree)))
			  (y (* radius (math/sin1 degree))))
		  (make-point (+ cx x) (+ cy y))))))


;-------------------------------------------------------------------------------
;
; shape circle
;
;-------------------------------------------------------------------------------
(defclass circle (shape)
  ((center-x	:initform   0 :initarg :center-x)	; number
   (center-y	:initform   0 :initarg :center-y)	; number
   (radius		:initform   0 :initarg :radius)		; number
   (fill		:initform nil :initarg :fill)		; (or nil fill-info)
   (stroke		:initform nil :initarg :stroke)))	; (or nil stroke-info)

(defmethod initialize-instance :after ((ent circle) &rest initargs)
  (declare (ignore initargs))
  (with-slots (fill stroke) ent
	(setf fill   (make-fill   (or fill   *default-fill*)))
	(setf stroke (make-stroke (or stroke *default-stroke*))))
  ent)

(defmethod check ((shp circle) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (center-x center-y radius fill stroke) shp
	(check-member center-x :nullable nil :types number)
	(check-member center-y :nullable nil :types number)
	(check-member radius   :nullable nil :types number)
	(check-object fill     canvas dict :nullable t :class   fill-info)
	(check-object stroke   canvas dict :nullable t :class stroke-info))
  (incf (slot-value shp 'center-x) (canvas-left canvas))
  (incf (slot-value shp 'center-y) (canvas-top  canvas))
  nil)

(defmethod shape-center ((shp circle))
  (slot-value shp 'center-x))

(defmethod shape-middle ((shp circle))
  (slot-value shp 'center-y))

(defmethod shape-width ((shp circle))
  (* 2 (slot-value shp 'radius)))

(defmethod shape-height ((shp circle))
  (* 2 (slot-value shp 'radius)))

(defmethod shape-top ((shp circle))
  (- (shape-middle shp) (slot-value shp 'radius)))

(defmethod shape-bottom ((shp circle))
  (+ (shape-middle shp) (slot-value shp 'radius)))

(defmethod shape-left ((shp circle))
  (- (shape-center shp) (slot-value shp 'radius)))

(defmethod shape-right ((shp circle))
  (+ (shape-center shp) (slot-value shp 'radius)))

(defmethod shape-connect-point ((shp circle) type arg)
  (circle-connect-point (shape-center  shp)
						(shape-middle  shp)
						(slot-value shp 'radius) type arg))
  
;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((shp circle)) ...)

;;MEMO : use impelementation of shape...
;;(defmethod entity-composition-p ((shp circle)) ...)
  
(defmethod draw-entity ((shp circle) writer)
  (let ((cls  (shape-class shp))
		(id   (and (not (entity-composition-p shp))
				   (slot-value shp 'id))))
	(pre-draw shp writer)
	(writer-write writer
				  "<circle "
				  (write-when id "id='" it "' ")
				  "cx='" (shape-center shp) "' "
				  "cy='" (shape-middle shp) "' "
				  "r='" (slot-value shp 'radius) "' "
				  (write-when cls "class='" it "' ")
				  (unless cls
					(let ((fill (slot-value shp 'fill)))
					  (when fill
						(to-property-strings fill))))
				  (unless cls
					(let ((strk (slot-value shp 'stroke)))
					  (when strk
						(to-property-strings strk))))
				  "/>")
	(post-draw shp writer))
  nil)


#|
#|EXPORT|#				:circle
 |#
(defmacro circle (x y radius
				  &key class fill stroke link layer id contents)
  (let ((code `(register-entity (make-instance 'diagram:circle
											   :center-x ,x :center-y ,y 
											   :radius ,radius :class ,class
											   :fill ,fill :stroke ,stroke
											   :link ,link :layer ,layer :id ,id))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (diagram:shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))

