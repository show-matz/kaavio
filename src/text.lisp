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
  ((x		:initform   0 :initarg :x)		; number
   (y		:initform   0 :initarg :y)		; number
   (text	:initform nil :initarg :text)	; string
   (align	:initform nil :initarg :align)	; keyword
   (class	:initform nil :initarg :class)	; keyword
   (font	:initform nil :initarg :font)	; (or nil font-info)
   (link	:initform nil :initarg :link)))	; (or nil link-info)


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
  (with-slots (x y text align class font link) txt
	(check-member   x     :nullable nil :types number)
	(check-member   y     :nullable nil :types number)
	(check-member   text  :nullable nil :types string)
	(check-member   align :nullable nil :types keyword)
	(check-member   class :nullable   t :types (or keyword string))
	(check-object   font  canvas dict :nullable nil :class font-info)
	(check-object   link  canvas dict :nullable   t :class link-info)
	(check-keywords align :left :center :right)
	(incf x (canvas-left canvas))
	(incf y (canvas-top  canvas)))
  nil)

(defmethod entity-composition-p ((txt text))
  (not (null (slot-value txt 'link))))

(defmethod pre-draw ((txt text) writer)
  (call-next-method)
  (when (entity-composition-p txt)
	(let ((lnk (slot-value txt 'link)))
	  (when lnk
		(write-link-open lnk writer)))))

(defmethod post-draw ((txt text) writer)
  (when (entity-composition-p txt)
	(let ((lnk (slot-value txt 'link)))
	  (when lnk
		(write-link-close lnk writer))))
  (call-next-method))

(defmethod draw-entity ((txt text) writer)
  (with-slots (x y align class font text) txt
	(let ((txt-anchor (ecase align
						((:left)   "start")
						((:center) "middle")
						((:right)  "end")))
		  (id  (and (not (entity-composition-p txt))
					(slot-value txt 'id))))
	  (pre-draw txt writer)
	  (writer-write writer
					"<text "
					(write-when id "id='" it "' ")
					"x='" x "' "
					"y='" y "' "
					"text-anchor='" txt-anchor "' "
					(write-when class "class='" it "' ")
					(unless class
					  (to-property-strings font))
					">" (escape-characters text) "</text>")
	  (post-draw txt writer))))



#|
#|EXPORT|#				:text
 |#
(defmacro text (x y text &key align class font link layer id)
  `(register-entity (make-instance 'diagram:text
								   :x ,x :y ,y :text ,text
								   :align ,align :class ,class :font ,font 
								   :link ,link :layer ,layer :id ,id)))

