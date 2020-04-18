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

(defun caluculate-shapesize (font text)
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

;-------------------------------------------------------------------------------
;
; shape paragraph
;
;-------------------------------------------------------------------------------
(defclass paragraph (shape)
  ((x		;:type     number
			:initform 0
			:initarg  :x
			:accessor paragraph-x)
   (y		;:type     number
			:initform 0
			:initarg  :y
			:accessor paragraph-y)
   (text	;:type     string -> list
			:initform nil
			:initarg  :text
			:accessor paragraph-text)
   (align	;:type     keyword
			:initform nil
			:initarg  :align
			:accessor paragraph-align)
   (valign	;:type     keyword
			:initform nil
			:initarg  :valign
			:accessor paragraph-valign)
   (font	;:type     (or nil font-info)
			:initform nil
			:initarg  :font
			:accessor paragraph-font)
   (width	;:type     number
			:initform nil
			:initarg  :width
			:accessor paragraph-width)
   (height	;:type     number
			:initform nil
			:initarg  :height
			:accessor paragraph-height)))


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
  (with-slots (x y text align valign font width height) shp
	(check-member   x      :nullable nil :types number)
	(check-member   y      :nullable nil :types number)
	(check-member   text   :nullable nil :types string)
	(check-member   align  :nullable nil :types keyword)
	(check-member   valign :nullable nil :types keyword)
	(check-object   font   canvas dict :nullable nil :class font-info)
	(check-keywords align  :left :center :right)
	(check-keywords valign :top  :center :bottom)
	(setf text (string/split text #\newline))
	(incf x (canvas-left canvas))
	(incf y (canvas-top  canvas))
	(multiple-value-bind (w h) (caluculate-shapesize font text)
	  (setf width  w)
	  (setf height h)))
  nil)

(defmethod shape-width ((shp paragraph))
  (paragraph-width shp))

(defmethod shape-height ((shp paragraph))
  (paragraph-height shp))

(defmethod shape-top ((shp paragraph))
  (with-slots (valign y height) shp
	(ecase valign
	  ((:top)    y)
	  ((:center) (- y (/ height 2)))
	  ((:bottom) (- y height)))))

(defmethod shape-middle ((shp paragraph))
  (with-slots (valign y height) shp
	(ecase valign
	  ((:top)    (+ y (/ height 2)))
	  ((:center) y)
	  ((:bottom) (- y (/ height 2))))))

(defmethod shape-bottom ((shp paragraph))
  (with-slots (valign y height) shp
	(ecase valign
	  ((:top)    (+ y height))
	  ((:center) (+ y (/ height 2)))
	  ((:bottom) y))))

(defmethod shape-left   ((shp paragraph))
  (with-slots (align x width) shp
	(ecase align
	  ((:left)   x)
	  ((:center) (- x (/ width 2)))
	  ((:right)  (- x width)))))

(defmethod shape-center ((shp paragraph))
  (with-slots (align x width) shp
	(ecase align
	  ((:left)   (+ x (/ width 2)))
	  ((:center) x)
	  ((:right)  (- x (/ width 2))))))

(defmethod shape-right  ((shp paragraph))
  (with-slots (align x width) shp
	(ecase align
	  ((:left)   (+ x width))
	  ((:center) (+ x (/ width 2)))
	  ((:right)  x))))

(defmethod entity-composition-p ((shp paragraph))
  (or (< 1 (length (paragraph-text shp)))
	  (call-next-method)))
  
(defmethod draw-entity ((shp paragraph) writer)
  (let ((x (paragraph-x shp))
		(y (shape-top shp))
		(txt-anchor (ecase (paragraph-align shp)
					  ((:left)   "start")
					  ((:center) "middle")
					  ((:right)  "end")))
		(cls (shape-class shp))
		(id   (and (not (entity-composition-p shp))
				   (slot-value shp 'id)))
		(font (paragraph-font shp)))
	(with-slots ((fsize size)
				 line-spacing) font
	  (let ((font-prop (to-property-strings font)))
		(pre-draw shp writer)
		(dolist (line (paragraph-text shp))
		  (incf y fsize)
		  (writer-write writer
						"<text "
						(write-when id "id='" it "' ")
						"x='" x "' "
						"y='" y "' "
						"text-anchor='" txt-anchor "' "
						(write-when cls "class='" it "' ")
						(unless cls font-prop)
						">" (escape-characters line) "</text>")
		  (incf y line-spacing))
		(post-draw shp writer))))
  nil)
					  


#|
#|EXPORT|#				:paragraph
 |#
(defmacro paragraph (x y text
					 &key align valign class font link layer id)
  `(register-entity (make-instance 'diagram:paragraph
								   :x ,x :y ,y :text ,text
								   :align ,align :valign ,valign :font ,font
								   :class ,class :link ,link :layer ,layer :id ,id)))

