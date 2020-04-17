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
;; label-info
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:label-info
 |#
(defclass label-info ()
  ((text	 ;:type     string
			 :initform ""
			 :initarg  :text
			 :accessor label-text)
   (position ;:type     keyword    ; :above|:below|:left|:right
			 :initform nil
			 :initarg  :position
			 :accessor label-position)
   (offset	 ;:type     number
			 :initform 0
			 :initarg  :offset
			 :accessor label-offset)
   (font	 ;:type     font-info
			 :initform nil
			 :initarg  :font
			 :accessor label-font)))


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
  (labels ((get-location-info ()
			 (let ((size (font-size (label-font label)))
				   (offset (label-offset label)))
			   (ecase (label-position label)
				 ((:above) (values "middle" (shape-center shp)
											(- (shape-top    shp) offset)))
				 ((:below) (values "middle" (shape-center shp)
											(+ (shape-bottom shp) offset size)))
				 ((:left)  (values "end"    (- (shape-left  shp) offset)
											(+ (shape-middle shp) (/ size 2))))
				 ((:right) (values "start"  (+ (shape-right shp) offset)
											(+ (shape-middle shp) (/ size 2))))))))
	(unless (typep shp 'shape)
	  (throw-exception "label-info : shp is not type of shape."))
	(multiple-value-bind (anchor x y) (get-location-info)
	  (let ((text (label-text label)))
		(setf text (escape-characters (if (stringp text)
										  text
										  (string-downcase (symbol-name text)))))
		(writer-write writer
					  "<text "
					  "x='" x "' "
					  "y='" y "' "
					  "text-anchor='" anchor "' "
					  (to-property-strings (label-font label))
					  ">" text "</text>")))))
  
  
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


