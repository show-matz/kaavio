#|
#|ASD|#				(:file "text"                      :depends-on ("cl-diagram"
#|ASD|#																"constants"
#|ASD|#																"entity"
#|ASD|#																"font-info"
#|ASD|#																"link-info"
#|ASD|#																"writer"))
#|EXPORT|#				;text.lisp
 |#


(in-package :cl-diagram)

;-------------------------------------------------------------------------------
;
; entity text
;
;-------------------------------------------------------------------------------
(defclass text (entity)
  ((x		;:type     number
			:initform 0
			:initarg  :x
			:accessor text-x)
   (y		;:type     number
			:initform 0
			:initarg  :y
			:accessor text-y)
   (text	;:type     string
			:initform nil
			:initarg  :text
			:accessor text-text)
   (align	;:type     keyword
			:initform nil
			:initarg  :align
			:accessor text-align)
   (class	;:type     keyword
			:initform nil
			:initarg  :class
			:accessor text-class)
   (font	;:type     (or nil font-info)
			:initform nil
			:initarg  :font
			:accessor text-font)
   (link	;:type     (or nil link-info)
			:initform nil
			:initarg  :link
			:accessor text-link)))


(defmethod initialize-instance :after ((txt text) &rest initargs)
  (declare (ignore initargs))
  (with-slots (align font link) txt
	(setf align (or align *default-text-align*))
	(setf font  (make-font (or font *default-font*)))
	(setf link  (make-link link)))
  txt)

(defmethod check ((txt text) canvas dict)
  (declare (ignorable dict))
  ;; this method must call super class' one.
  (call-next-method)
  (check-member   (x     (text-x txt))     :nullable nil :types number)
  (check-member   (y     (text-y txt))     :nullable nil :types number)
  (check-member   (text  (text-text  txt)) :nullable nil :types string)
  (check-member   (align (text-align txt)) :nullable nil :types keyword)
  (check-keywords (align (text-align txt)) :left :center :right)
  (check-member   (class (text-class txt)) :nullable   t :types (or keyword string))
  (check-object   (font  (text-font  txt)) canvas dict :nullable nil :class font-info)
  (check-object   (link  (text-link  txt)) canvas dict :nullable   t :class link-info)
  (incf (text-x txt) (canvas-left canvas))
  (incf (text-y txt) (canvas-top  canvas))
  nil)

(defmethod entity-composition-p ((txt text))
  (not (null (text-link txt))))

(defmethod pre-draw ((txt text) writer)
  (call-next-method)
  (when (entity-composition-p txt)
	(let ((lnk (text-link txt)))
	  (when lnk
		(write-link-open lnk writer)))))

(defmethod post-draw ((txt text) writer)
  (when (entity-composition-p txt)
	(let ((lnk (text-link txt)))
	  (when lnk
		(write-link-close lnk writer))))
  (call-next-method))

(defmethod draw-entity ((txt text) writer)
  (let ((txt-anchor (ecase (text-align txt)
					  ((:left)   "start")
					  ((:center) "middle")
					  ((:right)  "end")))
		(id  (and (not (entity-composition-p txt))
				  (entity-id txt))))
	(pre-draw txt writer)
	(writer-write writer
				  "<text "
				  (write-when id "id='" it "' ")
				  "x='" (text-x txt) "' "
				  "y='" (text-y txt) "' "
				  "text-anchor='" txt-anchor "' "
				  (write-when (text-class txt) "class='" it "' ")
				  (unless (text-class txt)
					(to-property-strings (text-font txt)))
				  ">" (escape-characters (text-text txt)) "</text>")
	(post-draw txt writer)))



#|
#|EXPORT|#				:text
 |#
(defmacro text (x y text &key align class font link layer id)
  `(register-entity (make-instance 'diagram:text
								   :x ,x :y ,y :text ,text
								   :align ,align :class ,class :font ,font 
								   :link ,link :layer ,layer :id ,id)))

