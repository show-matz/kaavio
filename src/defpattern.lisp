#|
#|ASD|#				(:file "defpattern"                :depends-on ("kaavio"
#|ASD|#				                                                "constants"
#|ASD|#				                                                "definition"
#|ASD|#				                                                "layer-manager"
#|ASD|#				                                                "dictionary"
#|ASD|#				                                                "point"
#|ASD|#				                                                "canvas"
#|ASD|#				                                                "font-info"
#|ASD|#				                                                "stroke-info"
#|ASD|#				                                                "writer"))
#|EXPORT|#				;defpattern.lisp
 |#

(in-package :kaavio)


;;-------------------------------------------------------------------------------
;;
;; class pattern-definition
;;
;;-------------------------------------------------------------------------------
#|
#|EXPORT|#				:pattern-definition
 |#
(defclass pattern-definition (definition)
  ((data			:initform  "" :initarg :data)	; string
   (x				:initform   0 :initarg :x)		; number
   (y				:initform   0 :initarg :y)		; number
   (width			:initform   0 :initarg :width)	; number
   (height			:initform   0 :initarg :height)	; number
   (units			:initform nil :initarg :units)	; keyword
   (content-units	:initform nil :initarg :content-units)	; keyword
   (view-box		:initform nil :initarg :view-box))); list


(defmethod check ((ptn pattern-definition) canvas dict)
  (declare (ignore canvas dict))
  (with-slots (data x y width height
			   units content-units view-box) ptn
	(check-member data   :nullable nil :types string)
	(check-member x      :nullable nil :types number)
	(check-member y      :nullable nil :types number)
	(check-member width  :nullable nil :types (or number string))
	(check-member height :nullable nil :types (or number string))
	(check-member units  :nullable nil :types keyword)
	(check-keywords units :userSpaceOnUse :objectBoundingBox)
    (check-member content-units :nullable t :types keyword)
	(when content-units
	  (check-keywords content-units :userSpaceOnUse :objectBoundingBox))
    (check-member view-box      :nullable t :types list))
  ;; this method must call super class' one.
  (call-next-method))

(defmethod entity-composition-p ((ptn pattern-definition))
  (declare (ignore ptn))
  t)

(defmethod pre-draw ((ptn pattern-definition) writer)
  (when (entity-composition-p ptn)
    (with-slots (id x y width height
			     units content-units view-box) ptn
	  (when id
		(labels ((units-tag (kwd)
				   (if (eq kwd :userSpaceOnUse)
					   "userSpaceOnUse"
					   "objectBoundingBox")))
		  (writer-write writer "<pattern id='" id "' x='" x "' y='" y "' "
							   "width='" width "' height='" height "' "
							   "patternUnits='" (units-tag units) "' "
							   (write-when content-units
							   			"patternContentUnits='" (units-tag it) "' ")
							   (write-when view-box
							   			"viewBox='" (format nil "~{ ~A~}" it) "' ")
							   ">"))
		(writer-incr-level writer)))))

(defmethod draw-entity ((ptn pattern-definition) writer)
  (with-slots (id data width height) ptn
	(writer-write writer "<defs>")
	(writer-incr-level writer)
	(pre-draw ptn writer)
	; data を 改行コードで区切ってリストにして、順番に出力
	(dolist (line (string/split data #\newline))
	  (writer-write writer line))
	(post-draw ptn writer)
	(writer-decr-level writer)
	(writer-write writer "</defs>"))
  nil)

(defmethod post-draw ((ptn pattern-definition) writer)
  (when (entity-composition-p ptn)
	(when (slot-value ptn 'id)
	  (writer-decr-level writer)
	  (writer-write writer "</pattern>"))))


;;------------------------------------------------------------------------------
;;
;; macro defpattern
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:defpattern
 |#
(defmacro defpattern ((width height id &key (x 0) (y 0)
						  (units :userSpaceOnUse) content-units view-box) &rest body)
  (let ((g-layer-mgr (gensym "LAYER-MGR"))
		(g-writer    (gensym "WRITER"))
		(g-entity    (gensym "ENTITY"))
		(g-entities  (gensym "ENTITIES"))
		(g-dict      (gensym "DICT")))
	`(let ((data (let ((,g-layer-mgr (layer-create-manager))
					   (,g-entities  nil)
					   (,g-dict      (dict-create *default-history-count*))
					   (canvas       (make-canvas (make-point 0 0 :absolute) ,width ,height)))
				   (declare (special canvas))
				   (labels ((layer (name &optional (display :inline))
							  (layer-register ,g-layer-mgr name display))
							(register-entity (,g-entity)
							  (unless (typep ,g-entity 'entity)
								(throw-exception "Can't register ~A to dictionary : NOT entity." ,g-entity))
							  (when (or (typep ,g-entity 'use)
										(typep ,g-entity 'definition))
								(throw-exception "Can't register ~A to dictionary in defpattern." ,g-entity))
							  (push ,g-entity ,g-entities)
							  (check ,g-entity canvas ,g-dict)
							  (dict-register ,g-dict ,g-entity)))
					 (declare (ignorable #'layer #'register-entity))
					 (let ((*default-font*   (or *default-font*   (make-font)))
						   (*default-fill*   (or *default-fill*   (make-fill)))
						   (*default-stroke* (or *default-stroke* (make-stroke))))
					   (with-dictionary ,g-dict
						 ,@body)))
				   ;; stable sort by priority of layer
				   (setf ,g-entities
						 (stable-sort (nreverse ,g-entities)
									  (lambda (e1 e2)
										(< (layer-get-priority ,g-layer-mgr (slot-value e1 'layer))
										   (layer-get-priority ,g-layer-mgr (slot-value e2 'layer))))))
				   (let ((,g-writer (create-svg-writer)))
					 (dolist (,g-entity ,g-entities)
					   (layer-change ,g-layer-mgr (slot-value ,g-entity 'layer) ,g-writer)
					   (write-header ,g-entity ,g-writer)
					   (draw-entity  ,g-entity ,g-writer))
					 (layer-change ,g-layer-mgr nil ,g-writer)
					 (writer-close ,g-writer)))))
	   (register-entity (make-instance 'kaavio::pattern-definition
									   :id ,id :data data :x ,x :y ,y
									   :width  ,width :height ,height :units ,units
									   :content-units ,content-units :view-box ,view-box)))))
									   
