#|
#|ASD|#				(:file "paragraph"                 :depends-on ("cl-diagram"
#|ASD|#																"constants"
#|ASD|#																"shape"
#|ASD|#																"font-info"
#|ASD|#																"link-info"
#|ASD|#																"point"
#|ASD|#																"writer"))
#|EXPORT|#				;paragraph.lisp
 |#


(in-package :cl-diagram)

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
   (rotate		:initform nil :initarg :rotate)		; (or nil number)
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
  (with-slots (position text align valign font width height) shp
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
	  (setf height h)))
  nil)

(defmethod shape-width ((shp paragraph))
  (slot-value shp 'width))

(defmethod shape-height ((shp paragraph))
  (slot-value shp 'height))

(defmethod shape-center ((shp paragraph))
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
  (with-slots (text rotate) shp
	(or rotate
		(< 1 (length text))
		(call-next-method))))
  
(defmethod shape-connect-point ((shp paragraph) type1 type2 arg)
  (with-slots (rotate) shp
	(unless (or (null rotate) (zerop rotate) (= 180 rotate))
	  (throw-exception "Can't connect to rotated paragraph.")))
  (call-next-method))
	
;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((shp rectangle)) ...)

(defmethod pre-draw ((shp paragraph) writer)
  (call-next-method)
  (with-slots (rotate) shp
	(when (and rotate (/= rotate 0))
	  (let ((pt (shape-center shp)))
		(writer-write writer "<g transform='rotate(" rotate ", "
									  (point-x pt) ", " (point-y pt) ")'>")
		(writer-incr-level writer)))))

(defmethod post-draw ((shp paragraph) writer)
  (with-slots (rotate) shp
	(when (and rotate (/= rotate 0))
	  (writer-decr-level writer)
	  (writer-write writer "</g>")))
  (call-next-method))


(defmethod draw-entity ((shp paragraph) writer)
  (with-slots (position class align font text) shp
	(let ((x (point-x position))
		  (y (point-y (shape-top shp)))
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
			(writer-write writer
						  "<text "
						  (write-when id "id='" it "' ")
						  "x='" x "' "
						  "y='" y "' "
						  "text-anchor='" txt-anchor "' "
						  (write-when class "class='" it "' ")
						  (unless class font-prop)
						  ">" (escape-characters line) "</text>")
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
					 &key align valign rotate class font link layer id)
  `(register-entity (make-instance 'diagram:paragraph
								   :position ,position :text ,text
								   :align ,align :valign ,valign :rotate ,rotate
								   :font ,font :class ,class :link ,link :layer ,layer :id ,id)))

