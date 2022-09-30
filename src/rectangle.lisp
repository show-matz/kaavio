#|
#|ASD|#				(:file "rectangle"                 :depends-on ("kaavio"
#|ASD|#																"constants"
#|ASD|#																"canvas"
#|ASD|#																"point"
#|ASD|#																"shape"
#|ASD|#																"stroke-info"
#|ASD|#																"link-info"
#|ASD|#																"filter"
#|ASD|#																"writer"))
#|EXPORT|#				;rectangle.lisp
 |#


(in-package :kaavio)

;;-------------------------------------------------------------------------------
;;
;; class rectangle
;;
;;-------------------------------------------------------------------------------
(defclass rectangle (shape)
  ((center		:initform nil :initarg :center)		; point
   (width		:initform   0 :initarg :width)		; number
   (height		:initform   0 :initarg :height)		; number
   (rx			:initform nil :initarg :rx)			; number
   (ry			:initform nil :initarg :ry)			; number
   (fill		:initform nil :initarg :fill)		; (or nil fill-info)
   (stroke		:initform nil :initarg :stroke)		; (or nil fill-info)
   (filter		:initform nil :initarg :filter)))	; (or nil keyword)


(defmethod initialize-instance :after ((rct rectangle) &rest initargs)
  (declare (ignore initargs))
  (with-slots (rx ry fill stroke filter layer) rct
	(setf rx     (or rx *default-rectangle-rx*))
	(setf ry     (or ry *default-rectangle-ry*))
	(when (and (null rx) ry)
		(setf rx ry))
	(when (and rx (null ry))
		(setf ry rx))
	(setf fill   (make-fill   (or fill   *default-fill*   :none)))
	(setf stroke (make-stroke (or stroke *default-stroke* :none)))
	(setf filter (if (eq filter :none)
					 nil
					 (or filter *default-shape-filter*)))
	(setf layer  (if (eq layer :none)
					 nil
					 (or layer *default-layer*))))
  rct)

(defmethod check ((rct rectangle) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (center width height rx ry fill stroke filter) rct
	(check-member width     :nullable nil :types number)
	(check-member height    :nullable nil :types number)
	(check-member rx        :nullable   t :types number)
	(check-member ry        :nullable   t :types number)
	(check-object fill      canvas dict :nullable nil :class   fill-info)
	(check-object stroke    canvas dict :nullable nil :class stroke-info)
	(check-member filter    :nullable   t :types keyword)
	(setf center (canvas-fix-point canvas center)))
  nil)

(defmethod shape-width ((rct rectangle))
  (slot-value rct 'width))

(defmethod shape-height ((rct rectangle))
  (slot-value rct 'height))

(defmethod shape-center ((rct rectangle))
  (slot-value rct 'center))

;;MEMO : use impelementation of shape...
;;(defmethod shape-connect-point ((shp rectangle) type1 type2 arg) ...)
  
;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((shp rectangle)) ...)

(defmethod draw-entity ((rct rectangle) writer)
  (with-slots (rx ry fill stroke filter) rct
	(let ((id (and (not (entity-composition-p rct))
				   (slot-value rct 'id)))
		  (topleft (shape-topleft rct)))
	  (pre-draw rct writer)
	  (writer-write writer
					"<rect "
					(write-when (keywordp id) "id='" id "' ")
					"x='" (point-x topleft) "' "
					"y='" (point-y topleft) "' "
					"width='"  (shape-width  rct) "' "
					"height='" (shape-height rct) "' "
					(write-when rx "rx='" it "' ")
					(write-when ry "ry='" it "' ")
					(to-property-strings fill)
					(to-property-strings stroke)
					(write-when filter "filter='url(#" it ")' ")
					"/>")
	  (post-draw rct writer)))
  nil)
  

;;-------------------------------------------------------------------------------
;;
;; macro rectangle
;;
;;-------------------------------------------------------------------------------
#|
#|EXPORT|#				:rectangle
 |#
(defmacro rectangle (center width height
					 &key rx ry fill stroke rotate link layer id filter contents)
  (let ((code `(register-entity (make-instance 'kaavio:rectangle
											   :center ,center
											   :width ,width :height ,height
											   :rx ,rx :ry ,ry
											   :fill ,fill :stroke ,stroke
											   :rotate ,rotate :link ,link
											   :filter ,filter :layer ,layer :id ,id))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (kaavio:shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))

#|
#|EXPORT|#				:rect
 |#
(defmacro rect (center width height
				&key rx ry fill stroke rotate link layer id filter contents)
  (let ((code `(register-entity (make-instance 'kaavio:rectangle
											   :center ,center
											   :width ,width :height ,height
											   :rx ,rx :ry ,ry
											   :fill ,fill :stroke ,stroke
											   :rotate ,rotate :link ,link
											   :filter ,filter :layer ,layer :id ,id))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (kaavio:shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))

