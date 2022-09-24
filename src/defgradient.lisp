#|
#|ASD|#				(:file "defgradient"               :depends-on ("kaavio"
#|ASD|#				                                                "definition"
#|ASD|#																"colormap"
#|ASD|#				                                                "writer"))
#|EXPORT|#				;defgradient.lisp
 |#

(in-package :kaavio)


;;-------------------------------------------------------------------------------
;;
;; class gradient-stop
;;
;;-------------------------------------------------------------------------------
#|
#|EXPORT|#				:gradient-stop
 |#
(defclass gradient-stop ()
  ((offset  :initform nil :initarg :offset)    ; "N%" or 0.0 ~ 1.0
   (color   :initform nil :initarg :color)     ; keyword
   (opacity :initform nil :initarg :opacity))) ; 0.0 ~ 1.0


(defmethod check ((stop gradient-stop) canvas dict)
  (declare (ignore canvas dict))
  (with-slots (offset color opacity) stop
	(check-member offset  :nullable nil :types (or string number))
	(check-member color   :nullable nil :types (or string keyword))
	(setf color (colormap-fix color))
	(check-member opacity :nullable t :types number))
  t)

(defun write-gradient-stop (writer stop)
  (let ((offset  (slot-value stop 'offset))
		(color   (slot-value stop 'color))
		(opacity (slot-value stop 'opacity)))
	(writer-write writer
				  "<stop offset='" offset "' " 
						"stop-color='" color "' " 
					(write-when opacity
						"stop-opacity='" opacity "' ")
				  "/>")))


;;-------------------------------------------------------------------------------
;;
;; class gradient-definition
;;
;;-------------------------------------------------------------------------------
#|
#|EXPORT|#				:gradient-definition
 |#
(defclass gradient-definition (definition)
  ((id        :initform nil :initarg :id)          ; keyword
   (stops     :initform nil :initarg :stops)       ; list of list => list of gradient-stop object.
   (href      :initform nil :initarg :href)        ; keyword
   (units     :initform nil :initarg :units)       ; keyword
   (spread    :initform nil :initarg :spread)      ; spreadMethod: :pad :repeat :reflect
   (transform :initform nil :initarg :transform))) ; string


(defmethod check ((obj gradient-definition) canvas dict)
  (declare (ignore canvas dict))
  (with-slots (id stops href units spread transform) obj
	(check-member id     :nullable nil :types keyword)
	(setf stops (mapcar (lambda (lst)
						  (destructuring-bind (offset color &optional opacity) lst
							(make-instance 'gradient-stop
										   :offset  offset
										   :color   color
										   :opacity opacity))) stops))
	(dolist (stop stops)
	  (check stop canvas dict))
	(check-member href   :nullable   t :types keyword)
	(check-member units  :nullable   t :types keyword)
	(when units
	  (check-keywords units :objectBoundingBox :userSpaceOnUse))
	(check-member spread :nullable   t :types keyword)
	(when spread
	  (check-keywords spread :pad :repeat :reflect))
	(check-member transform :nullable t :types string))
  ;; this method must call super class' one.
 (call-next-method))


(defmethod entity-composition-p ((obj gradient-definition))
  (declare (ignore obj))
  t)

(defmethod draw-entity ((obj gradient-definition) writer)
  (with-slots (stops) obj
	(writer-write writer "<defs>")
	(writer-incr-level writer)
	(pre-draw obj writer)
	; stops を順番に出力
	(dolist (stop stops)
	  (write-gradient-stop writer stop))
	(post-draw obj writer)
	(writer-decr-level writer)
	(writer-write writer "</defs>"))
  nil)


;;-------------------------------------------------------------------------------
;;
;; class linear-gradient-definition
;;
;;-------------------------------------------------------------------------------
#|
#|EXPORT|#				:linear-gradient-definition
 |#
(defclass linear-gradient-definition (gradient-definition)
  ((x1     :initform nil :initarg :x1)
   (y1     :initform nil :initarg :y1)
   (x2     :initform nil :initarg :x2)
   (y2     :initform nil :initarg :y2)))

(defmethod check ((obj linear-gradient-definition) canvas dict)
  (declare (ignore canvas dict))
  ;; this method must call super class' one.
  (call-next-method)
  ;;ToDo : implement...
)

(defmethod pre-draw ((obj linear-gradient-definition) writer)
  (with-slots (id stops href units spread transform x1 y1 x2 y2) obj
	(labels ((units-tag (kwd)
			   (if (eq kwd :userSpaceOnUse)
				   "userSpaceOnUse"
				   "objectBoundingBox")))
	  (writer-write writer "<linearGradient "
							  "id='" id "' "
							  (write-when href      "xlink:href='#" it "' ")
							  (write-when units     "gradientUnits='" (units-tag it) "' ")
							  (write-when spread    "spreadMethod='" it "' ")
							  (write-when transform "gradientTransform='" it "' ")
							  (write-when x1        "x1='" it "' ")
							  (write-when y1        "y1='" it "' ")
							  (write-when x2        "x2='" it "' ")
							  (write-when y2        "y2='" it "' ")
							  (if stops ">" "/>"))))
  (writer-incr-level writer))

(defmethod post-draw ((obj linear-gradient-definition) writer)
  (writer-decr-level writer)
  (with-slots (stops) obj
	(when stops
	  (writer-write writer "</linearGradient>"))))


;;-------------------------------------------------------------------------------
;;
;; class radial-gradient-definition
;;
;;-------------------------------------------------------------------------------
#|
#|EXPORT|#				:radial-gradient-definition
 |#
(defclass radial-gradient-definition (gradient-definition)
  ((cx     :initform nil :initarg :cx)    
   (cy     :initform nil :initarg :cy)    
   (fx     :initform nil :initarg :fx)    
   (fy     :initform nil :initarg :fy)    
   (radius :initform nil :initarg :radius)))

(defmethod check ((obj radial-gradient-definition) canvas dict)
  (declare (ignore canvas dict))
  ;; this method must call super class' one.
  (call-next-method)
  ;;ToDo : implement...
)

(defmethod pre-draw ((obj radial-gradient-definition) writer)
  (with-slots (id stops href units spread transform cx cy fx fy radius) obj
	(labels ((units-tag (kwd)
			   (if (eq kwd :userSpaceOnUse)
				   "userSpaceOnUse"
				   "objectBoundingBox")))
	  (writer-write writer "<radialGradient "
							  "id='" id "' "
							  (write-when href      "xlink:href='#" it "' ")
							  (write-when units     "gradientUnits='" (units-tag it) "' ")
							  (write-when spread    "spreadMethod='" it "' ")
							  (write-when transform "gradientTransform='" it "' ")
							  (write-when cx        "cx='" it "' ")
							  (write-when cy        "cy='" it "' ")
							  (write-when fx        "fx='" it "' ")
							  (write-when fy        "fy='" it "' ")
							  (write-when radius     "r='" it "' ")
							  (if stops ">" "/>"))))
  (writer-incr-level writer))

(defmethod post-draw ((obj radial-gradient-definition) writer)
  (writer-decr-level writer)
  (with-slots (stops) obj
	(when stops
	  (writer-write writer "</radialGradient>"))))



;;------------------------------------------------------------------------------
;;
;; macro defgradient
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:defgradient
 |#
(defmacro defgradient ((type id &rest params) &rest stops)
  (ecase type
	((:linear) `(register-entity
					 (make-instance 'linear-gradient-definition
									:id ,id :stops ',stops ,@params)))
	((:radial) `(register-entity
					 (make-instance 'radial-gradient-definition
									:id ,id :stops ',stops ,@params)))))

