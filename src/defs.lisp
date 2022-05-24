#|
#|ASD|#				(:file "defs"                      :depends-on ("cl-diagram"
#|ASD|#				                                                "constants"
#|ASD|#																"entity"
#|ASD|#				                                                "layer-manager"
#|ASD|#				                                                "dictionary"
#|ASD|#				                                                "point"
#|ASD|#				                                                "canvas"
#|ASD|#				                                                "font-info"
#|ASD|#				                                                "stroke-info"
#|ASD|#				                                                "writer"))
#|EXPORT|#				;defs.lisp
 |#

(in-package :cl-diagram)


;;-------------------------------------------------------------------------------
;;
;; class defs
;;
;;-------------------------------------------------------------------------------
(defclass defs (entity)
  ((data	:initform  "" :initarg :data)		; string
   (width	:initform   0 :initarg :width)		; number
   (height	:initform   0 :initarg :height)))	; number


(defmethod check ((ent defs) canvas dict)
  (declare (ignore canvas dict))
  (with-slots (data width height) ent
	(check-member data   :nullable nil :types string)
	(check-member width  :nullable nil :types number)
	(check-member height :nullable nil :types number))
  ;; this method must call super class' one.
  (call-next-method))

(defmethod entity-composition-p ((ent defs))
  (declare (ignore ent))
  t)

(defmethod draw-entity ((ent defs) writer)
  (with-slots (id data width height) ent
	(writer-write writer "<defs>")
	(writer-incr-level writer)
	(pre-draw ent writer)
	; data を 改行コードで区切ってリストにして、順番に出力
	(dolist (line (string/split data #\newline))
	  (writer-write writer line))
	(post-draw ent writer)
	(writer-decr-level writer)
	(writer-write writer "</defs>"))
  nil)

;;------------------------------------------------------------------------------
;;
;; macro defs
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:defs
 |#
(defmacro defs ((width height id) &rest body)
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
										(typep ,g-entity 'defs))
								(throw-exception "Can't register ~A to dictionary in defs." ,g-entity))
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
	   (register-entity (make-instance 'diagram::defs
									   :id     ,id
									   :data   data
									   :width  ,width
									   :height ,height)))))
									   


