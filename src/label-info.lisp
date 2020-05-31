#|
#|ASD|#				(:file "label-info"                :depends-on ("cl-diagram"
#|ASD|#																"constants"
#|ASD|#																"point"
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
   (offset	 :initform nil :initarg :offset)	; (or nil list)
   (font	 :initform nil :initarg :font)))	; font-info


(defmethod initialize-instance :after ((label label-info) &rest initargs)
  (declare (ignore initargs))
  (with-slots (position font offset) label
	(setf position (or position *default-label-position*))
	(setf   offset (or   offset (make-point 0 0)))
	(setf     font (make-font (or font *default-label-font* *default-font*))))
  label)


(defmethod check ((ent label-info) canvas dict)
  (with-slots (text position offset font) ent
	(check-member text     :nullable nil :types (or keyword string))
	(check-member position :nullable nil :types keyword)
	(check-member offset   :nullable   t :types cons)
	(when offset
	  (with-point (x y) offset
		(check-member x    :nullable nil :types number)
		(check-member y    :nullable nil :types number)))
	(check-object font     canvas dict :nullable nil :class font-info)
	(check-keywords position :above :below :left :right))
  t)

(defun label-info-locate-text-for-above (shp offset lines font-size spacing)
  (let* ((pt     (shape-top shp))
		 (cnt    (length lines))
		 (height (+ (* (1- cnt) font-size) (* cnt spacing))))
	(values "middle"
			(point+ (point/y+ pt (- height)) offset))))

(defun label-info-locate-text-for-below (shp offset lines font-size spacing)
  (declare (ignore lines))
  (let* ((pt (shape-bottom shp)))
	(values "middle"
			(point+ (point/y+ pt (+ spacing font-size)) offset))))

(defun label-info-locate-text-for-left (shp offset lines font-size spacing)
  (let* ((pt     (shape-left shp))
		 (cnt    (length lines))
		 (height (+ (* cnt font-size) (* (1- cnt) spacing))))
	(values "end"
			(point+ (point/y+ pt (- font-size (/ height 2))) offset))))

(defun label-info-locate-text-for-right (shp offset lines font-size spacing)
  (let* ((pt     (shape-right shp))
		 (cnt    (length lines))
		 (height (+ (* cnt font-size) (* (1- cnt) spacing))))
	(values "start"
			(point+ (point/y+ pt (- font-size (/ height 2))) offset))))
  
  
#|
#|EXPORT|#				:draw-label-with-point
 |#
(defun draw-label-with-point (label x y anchor writer)
  (with-slots (text offset font) label
	(let* ((size    (slot-value font 'size))
		   (spacing (slot-value font 'line-spacing))
		   (lines   (string/split (escape-characters (fix-name text)) #\newline))
		   (cnt     (length lines))
		   (height  (+ (* cnt size) (* (1- cnt) spacing)))
		   (x          (+ x (point-x offset)))
		   (y       (- (+ y (point-y offset)) (/ height 2))))
	  (dolist (line lines)
		(incf y size)
		(writer-write writer
					  "<text "
					  "x='" x "' "
					  "y='" y "' "
					  "text-anchor='" anchor "' "
					  (to-property-strings font)
					  ">" line "</text>")
		(incf y spacing)))))

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
				 (let ((locater (ecase position
								  ((:above) #'label-info-locate-text-for-above)
								  ((:below) #'label-info-locate-text-for-below)
								  ((:left)  #'label-info-locate-text-for-left )
								  ((:right) #'label-info-locate-text-for-right))))
				   (funcall locater shp offset lines size spacing))))
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


