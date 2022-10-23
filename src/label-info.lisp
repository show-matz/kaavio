#|
#|ASD|#				(:file "label-info"                :depends-on ("kaavio"
#|ASD|#																"constants"
#|ASD|#																"point"
#|ASD|#																"canvas"
#|ASD|#																"font-info"
#|ASD|#																"shape"
#|ASD|#																"writer"))
#|EXPORT|#				;label-info.lisp
 |#

(in-package :kaavio)

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
	(setf   offset (or offset   *default-label-offset* (make-point 0 0)))
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
(defun draw-label-with-point (label x y sin cos writer)
  (with-slots (text offset font) label
	(labels ((calc-width-and-height ()
			   (let* ((width     0)
					  (height    0)
					  (lines     (fix-name text))
					  (font-size (slot-value font 'kaavio::size))
					  (spacing   (slot-value font 'kaavio::line-spacing)))
				 (multiple-value-setq (width height)
									  (font-calc-textarea font lines))
				 (values width height font-size
						 spacing (string/split lines #\newline)))))
	  (multiple-value-bind (width height
							font-size spacing lines) (calc-width-and-height)
		;(format t "x:~A, y:~A, sin:~A, cos:~A, width=~A, height~A~%" x y sin cos width height)
		(let ((x (+ x (* sin (/ height 2))))
			  (y (- y (* cos (/ height 2)))))
		  (decf y (/ height 2))
		  (when (< 0 sin) (incf x (/ width 2)))
		  (when (< sin 0) (decf x (/ width 2)))
		  (dolist (line lines)
			(incf y font-size)
			(write-text-tag (+ x (point-x offset))
							(+ y (point-y offset)) "middle" line writer :font font)
			(incf y spacing)))))))

#|
#|EXPORT|#				:draw-label
 |#
(defun draw-label (label shp writer)
  (with-slots (text offset position font) label
	(let ((size    (slot-value font 'size))
		  (spacing (slot-value font 'line-spacing))
		  (lines   (string/split (fix-name text) #\newline)))
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
			(write-text-tag (point-x pt)
							(point-y pt) anchor line writer :font font)
			(incf (point-y pt) (+ spacing size))))))))
  
  
#|
#|EXPORT|#				:make-label
 |#
(defun make-label (&rest params)
  (if (= 1 (length params))
	  (let ((param (car params)))
		(cond
		  ((typep param 'label-info) param)
		  ((keywordp param) (make-label param :offset nil))
		  ((stringp  param) (make-label param :offset nil))
		  ((listp    param) (apply #'make-label param))
		  (t                (make-label param :offset nil))))
	  (if (null params)
		  nil
		  (destructuring-bind (text &key position offset font) params
			(make-instance 'label-info
						   :text     text   :position position
						   :offset   offset :font     font)))))


;;------------------------------------------------------------------------------
;;
;; macro with-label-options
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:with-label-options
 |#
(defmacro with-label-options ((&key position offset font) &rest body)
  (labels ((impl (params acc)
			 (if (null params)
				 acc
				 (let ((value  (car  params))
					   (symbol (cadr params)))
				   (impl (cddr params)
						 (if (null value)
							 acc
							 (push (list symbol value) acc)))))))
	(let ((lst (impl (list position '*default-label-position*
						   offset   '*default-label-offset*
						   font     '*default-label-font*) nil)))
	  `(let ,lst
		 ,@body))))
