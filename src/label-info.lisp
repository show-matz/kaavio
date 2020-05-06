#|
#|ASD|#				(:file "label-info"                :depends-on ("cl-diagram"
#|ASD|#																"constants"
#|ASD|#																"canvas"
#|ASD|#																"font-info"
#|ASD|#																"shape"
#|ASD|#																"writer"))
#|EXPORT|#				;label-info.lisp
 |#

(in-package :cl-diagram)

;;------------------------------------------------------------------------------
;;
;; class label-info
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:label-info
 |#
(defclass label-info ()
  ((text	 :initform  "" :initarg :text)		; (or keyword string)
   (position :initform nil :initarg :position)	; keyword - :above :below :left :right
   (offset	 :initform   0 :initarg :offset)	; number
   (font	 :initform nil :initarg :font)))	; font-info


(defmethod initialize-instance :after ((label label-info) &rest initargs)
  (declare (ignore initargs))
  (with-slots (position font offset) label
	(setf position (or position *default-label-position*))
	(setf   offset (or   offset *default-label-offset*))
	(setf     font (make-font (or font *default-label-font* *default-font*))))
  label)


(defmethod check ((ent label-info) canvas dict)
  (with-slots (text position offset font) ent
	(check-member text     :nullable nil :types (or keyword string))
	(check-member position :nullable nil :types keyword)
	(check-member offset   :nullable nil :types number)
	(check-object font     canvas dict :nullable nil :class font-info)
	(check-keywords position :above :below :left :right))
  t)

  
#|
#|EXPORT|#				:draw-label
 |#
(defun draw-label (label shp writer)
  (with-slots (text offset position font) label
	(let ((size    (slot-value font 'size))
		  (spacing (slot-value font 'line-spacing))
		  (lines   (string/split (escape-characters (fix-name text)) #\newline)))
	  (unless (typep shp 'shape)
		(throw-exception "label-info : shp is not type of shape."))
	  (labels ((get-location-info ()
				 (ecase position
				   ((:above) (values "middle" (point/y+  (shape-top    shp) (- offset))))
				   ((:below) (values "middle" (point/y+  (shape-bottom shp) (+ offset     size))))
				   ((:left)  (values "end"    (point/xy+ (shape-left   shp) (- offset) (/ size 2))))
				   ((:right) (values "start"  (point/xy+ (shape-right  shp)    offset  (/ size 2)))))))
		(multiple-value-bind (anchor pt) (get-location-info)
		  (dolist (line lines)
			(writer-write writer
						  "<text "
						  "x='" (point-x pt) "' "
						  "y='" (point-y pt) "' "
						  "text-anchor='" anchor "' "
						  (to-property-strings font)
						  ">" line "</text>")
			(incf (point-y pt) (+ spacing size))))))))
  
  
#|
#|EXPORT|#				:make-label
 |#
(defun make-label (&rest params)
  (if (= 1 (length params))
	  (let ((param (car params)))
		(cond
		  ((typep param 'label-info) param)
		  ((keywordp param) (make-label :text param))
		  ((stringp  param) (make-label :text param))
		  ((listp    param) (apply #'make-label param))
		  (t                (make-label :text param))))
	  (if (null params)
		  nil
		  (destructuring-bind (&key text position offset font) params
			(make-instance 'label-info
						   :text     text   :position position
						   :offset   offset :font     font)))))


;;(make-label)
;;(make-label "LABEL"))
;;(make-label :name "LABEL" :position :left)
;;(make-label '(:name "LABEL" :position :left))


