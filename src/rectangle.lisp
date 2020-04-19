#|
#|ASD|#				(:file "rectangle"                 :depends-on ("cl-diagram"
#|ASD|#																"constants"
#|ASD|#																"canvas"
#|ASD|#																"point"
#|ASD|#																"shape"
#|ASD|#																"stroke-info"
#|ASD|#																"link-info"
#|ASD|#																"writer"))
#|EXPORT|#				;rectangle.lisp
 |#


(in-package :cl-diagram)

;-------------------------------------------------------------------------------
;
; shape rectangle
;
;-------------------------------------------------------------------------------
(defclass rectangle (shape)
  ((center-x	:initform   0 :initarg :center-x)	; number
   (center-y	:initform   0 :initarg :center-y)	; number
   (width		:initform   0 :initarg :width)		; number
   (height		:initform   0 :initarg :height)		; number
   (rx			:initform nil :initarg :rx)			; number
   (ry			:initform nil :initarg :ry)			; number
   (fill		:initform nil :initarg :fill)		; (or nil fill-info)
   (stroke		:initform nil :initarg :stroke)))	; (or nil link-info)


(defmethod initialize-instance :after ((rct rectangle) &rest initargs)
  (declare (ignore initargs))
  (with-slots (rx ry fill stroke) rct
	(unless rx
	  (setf rx *default-rectangle-rx*))
	(unless ry
	  (setf ry *default-rectangle-ry*))
	(setf fill (if (null fill)
				   *default-fill*
				   (make-fill fill)))
	(setf stroke (if (null stroke)
					 *default-stroke*
					 (make-stroke stroke))))
  rct)

(defmethod check ((rct rectangle) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (center-x center-y width height rx ry fill stroke) rct
	(check-member center-x  :nullable nil :types number)
	(check-member center-y  :nullable nil :types number)
	(check-member width     :nullable nil :types number)
	(check-member height    :nullable nil :types number)
	(check-member rx        :nullable   t :types number)
	(check-member ry        :nullable   t :types number)
	(check-object fill      canvas dict :nullable t :class   fill-info)
	(check-object stroke    canvas dict :nullable t :class stroke-info))
  (incf (slot-value rct 'center-x) (canvas-left canvas))
  (incf (slot-value rct 'center-y) (canvas-top  canvas))
  nil)

(defmethod shape-center ((rct rectangle))
  (slot-value rct 'center-x))

(defmethod shape-middle ((rct rectangle))
  (slot-value rct 'center-y))

(defmethod shape-width ((rct rectangle))
  (slot-value rct 'width))

(defmethod shape-height ((rct rectangle))
  (slot-value rct 'height))

;;MEMO : use impelementation of shape...
;;(defmethod shape-connect-point ((shp rectangle) type arg) ...)
  
;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((shp rectangle)) ...)

(defmethod draw-entity ((rct rectangle) writer)
  (with-slots (rx ry fill stroke class) rct
	(let ((id (and (not (entity-composition-p rct))
				   (slot-value rct 'id))))
	  (pre-draw rct writer)
	  (writer-write writer
					"<rect "
					(write-when id "id='" it "' ")
					"x='" (shape-left rct) "' "
					"y='" (shape-top  rct) "' "
					"width='"  (shape-width  rct) "' "
					"height='" (shape-height rct) "' "
					(write-when rx "rx='" it "' ")
					(write-when ry "ry='" it "' ")
					(write-when class "class='" it "' ")
					(unless class
					  (when fill
						(to-property-strings fill)))
					(unless class
					  (when stroke
						(to-property-strings stroke)))
					"/>")
	  (post-draw rct writer)))
  nil)
  

#|
#|EXPORT|#				:rectangle
 |#
(defmacro rectangle (x y width height
					 &key rx ry class fill stroke link layer id contents)
  (let ((code `(register-entity (make-instance 'diagram:rectangle
											   :center-x ,x :center-y ,y
											   :width ,width :height ,height
											   :rx ,rx :ry ,ry :class ,class
											   :fill ,fill :stroke ,stroke
											   :link ,link :layer ,layer :id ,id))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (diagram:shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))

