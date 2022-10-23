#|
#|ASD|#				(:file "paragraph"                 :depends-on ("kaavio"
#|ASD|#																"constants"
#|ASD|#																"text"
#|ASD|#																"shape"
#|ASD|#																"font-info"
#|ASD|#																"link-info"
#|ASD|#																"point"
#|ASD|#																"writer"))
#|EXPORT|#				;paragraph.lisp
 |#


(in-package :kaavio)

(defun caluculate-paragraph-shapesize (font text)
  (with-slots (size
			   (spice width-spice)
			   (spacing line-spacing)) font
	(let ((line-count (length text))
		  (width-fnc  (lambda (line)
						(* (length line) size spice))))    ;ToDo : what can I do ?
	  ;;ToDo : implement... fix width-fnc.
	  (values (apply #'max (mapcar width-fnc text))
			  (+ (* size line-count)
				 (* spacing (1- line-count)))))))

;;------------------------------------------------------------------------------
;;
;; class paragraph
;;
;;------------------------------------------------------------------------------
(defclass paragraph (shape)
  ((position	:initform nil :initarg :position)	; point
   (text		:initform nil :initarg :text)		; string -> list
   (align		:initform nil :initarg :align)		; keyword
   (valign		:initform nil :initarg :valign)		; keyword
   (font		:initform nil :initarg :font)		; (or nil font-info)
   (width		:initform nil :initarg :width)		; number
   (height		:initform nil :initarg :height)))	; number


(defmethod initialize-instance :after ((shp paragraph) &rest initargs)
  (declare (ignore initargs))
  (with-slots (align valign font) shp
	(setf align  (or align  *default-paragraph-align*))
	(setf valign (or valign *default-paragraph-valign*))
	(setf font   (make-font (or font *default-font*))))
  shp)

(defmethod check ((shp paragraph) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (position text align valign font width height layer) shp
	(check-member   text   :nullable nil :types string)
	(check-member   align  :nullable nil :types keyword)
	(check-member   valign :nullable nil :types keyword)
	(check-object   font   canvas dict :nullable nil :class font-info)
	(check-keywords align  :left :center :right)
	(check-keywords valign :top  :center :bottom)
	(setf text (string/split (fix-name text) #\newline))
	(setf position (canvas-fix-point canvas position))
	(multiple-value-bind (w h) (caluculate-paragraph-shapesize font text)
	  (setf width  w)
	  (setf height h))
	(setf layer  (if (eq layer :none)
					 nil
					 (or layer *default-layer*))))
  nil)

(defmethod attribute-width ((shp paragraph))
  (slot-value shp 'width))

(defmethod attribute-height ((shp paragraph))
  (slot-value shp 'height))

(defmethod attribute-center ((shp paragraph))
  (with-slots (position width height align valign) shp
	(point/xy+ position
			   (ecase align
				 ((:left)   (/ width 2))
				 ((:center) 0)
				 ((:right)  (- (/ width 2))))
			   (ecase valign
				 ((:top)    (/ height 2))
				 ((:center) 0)
				 ((:bottom) (- (/ height 2)))))))

(defmethod entity-composition-p ((shp paragraph))
  (with-slots (text) shp
	(or (< 1 (length text))
		(call-next-method))))
  
;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((shp rectangle)) ...)

(defmethod draw-entity ((shp paragraph) writer)
  (with-slots (position align font text) shp
	(let ((x (point-x position))
		  (y (point-y (attribute-top shp)))
		  (txt-anchor (ecase align
						((:left)   "start")
						((:center) "middle")
						((:right)  "end")))
		  (id   (and (not (entity-composition-p shp))
					 (slot-value shp 'id))))
	  (with-slots ((fsize size)
				   line-spacing) font
		(let ((font-prop (to-property-strings font)))
		  (pre-draw shp writer)
		  (dolist (line text)
			(incf y fsize)
			(write-text-tag x y txt-anchor line writer :id id :font font-prop)
			(incf y line-spacing))
		  (post-draw shp writer)))))
  nil)
					  


;;------------------------------------------------------------------------------
;;
;; macro paragraph
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:paragraph
 |#
(defmacro paragraph (position text
					 &key align valign rotate font link layer id)
  `(register-entity (make-instance 'kaavio:paragraph
								   :position ,position :text ,text
								   :align ,align :valign ,valign :rotate ,rotate
								   :font ,font :link ,link :layer ,layer :id ,id)))

