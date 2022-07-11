#|
#|ASD|#				(:file "text"                      :depends-on ("cl-diagram"
#|ASD|#																"constants"
#|ASD|#																"entity"
#|ASD|#																"font-info"
#|ASD|#																"link-info"
#|ASD|#																"filter"
#|ASD|#																"writer"))
#|EXPORT|#				;text.lisp
 |#


(in-package :cl-diagram)


(defun need-preserve-space-p (txt &optional (start 0))
  (labels ((somewhat-space-p (c)
			 (or (char= c #\space) (char= c #\tab))))
	(let ((idx (position-if #'somewhat-space-p txt :start start)))
	  (unless (or (null idx)
				  (= idx (1- (length txt))))
		(if (somewhat-space-p (char txt (1+ idx)))
			idx
			(need-preserve-space-p txt (1+ idx)))))))

#|
#|EXPORT|#				:write-text-tag
 |#
(defun write-text-tag (x y anchor txt writer &key id font filter)
  (writer-write writer
				"<text x='" x "' y='" y "' "
				(write-when id "id='" it "' ")
				"text-anchor='" anchor "' "
				(if (stringp font)
					font
					(when font
					  (to-property-strings font)))
				(when (need-preserve-space-p txt)
				  "xml:space='preserve' ")
				(write-when filter "filter='url(#" it ")' ")
				">" (escape-characters txt) "</text>"))

;;------------------------------------------------------------------------------
;;
;; class text
;;
;;------------------------------------------------------------------------------
(defclass text (entity)
  ((position	:initform nil :initarg :position)	; number
   (text		:initform nil :initarg :text)		; string
   (align		:initform nil :initarg :align)		; keyword
   (font		:initform nil :initarg :font)		; (or nil font-info)
   (link		:initform nil :initarg :link)		; (or nil link-info)
   (filter		:initform nil :initarg :filter)))	; (or nil keyword)


(defmethod initialize-instance :after ((txt text) &rest initargs)
  (declare (ignore initargs))
  (with-slots (align font link filter) txt
	(setf align (or align *default-text-align*))
	(setf font  (make-font (or font *default-font*)))
	(setf link  (make-link link))
	(setf filter (if (eq filter :none)
					 nil
					 (or filter *default-text-filter*))))
  txt)

(defmethod check ((txt text) canvas dict)
  (declare (ignorable dict))
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (position text align font link filter) txt
	(check-member   text  :nullable nil :types string)
	(check-member   align :nullable nil :types keyword)
	(check-object   font  canvas dict :nullable nil :class font-info)
	(check-object   link  canvas dict :nullable   t :class link-info)
	(check-keywords align :left :center :right)
	(check-member   filter :nullable  t :types keyword)
	(setf position (canvas-fix-point canvas position)))
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
  (with-slots (position align font text filter) txt
	(let ((txt-anchor (ecase align
						((:left)   "start")
						((:center) "middle")
						((:right)  "end")))
		  (id  (and (not (entity-composition-p txt))
					(slot-value txt 'id))))
	  (pre-draw txt writer)
	  (write-text-tag (point-x position)
					  (point-y position)
					  txt-anchor text writer
					  :id id :font font :filter filter)
	  (post-draw txt writer))))



;;------------------------------------------------------------------------------
;;
;; macro text
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:text
 |#
(defmacro text (position text &key align font link layer filter id)
  `(register-entity (make-instance 'diagram:text
								   :position ,position :text ,text
								   :align ,align :font ,font 
								   :link ,link :filter ,filter :layer ,layer :id ,id)))

