#|
#|ASD|#				(:file "cylinder"                  :depends-on ("cl-diagram"
#|ASD|#																"path"
#|ASD|#																"filter"
#|ASD|#																"text-shape"))
#|EXPORT|#				;cylinder.lisp
 |#

(in-package :cl-diagram)

;;------------------------------------------------------------------------------
;;
;; class cylinder
;;
;;------------------------------------------------------------------------------
(defclass cylinder (text-shape)
  ((depth	:initform nil :initarg :depth)    ; number
   (filter	:initform nil :initarg :filter))) ; (or nil keyword)
  
(defmethod initialize-instance :after ((cyl cylinder) &rest initargs)
  (declare (ignore initargs))
  (with-slots (filter) cyl
	(setf filter (or filter *default-shape-filter* *default-filter*)))
  cyl)
   
(defmethod check ((cyl cylinder) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (depth filter height) cyl
	(setf depth (or depth (/ height 5)))
	(check-member depth  :nullable nil :types number)
	(check-member filter :nullable   t :types keyword))
  nil)

;; override of group::draw-group
(defmethod draw-group ((cyl cylinder) writer)
  (let* ((canvas (group-get-canvas cyl))
		 (w      (canvas-width  canvas))
		 (h      (canvas-height canvas)))
	(macrolet ((register-entity (entity)
				 `(check-and-draw-local-entity ,entity canvas writer)))
	  (with-slots (depth fill stroke filter) cyl
		;; draw 
		(path `((:move-to (0  0))
				(:line-to (0 ,h))
				(:arc-to ,(/ w 2) ,(/ depth 2) 0 0 0 (,w ,h))
				(:line-to (,w 0))
				(:arc-to ,(/ w 2) ,(/ depth 2) 0 0 0 (0 0))) :fill fill :stroke stroke :filter filter)
		(path `((:move-to (0  0))
				(:arc-to ,(/ w 2) ,(/ depth 2) 0 0 0 (,w 0))) :fill :none :stroke stroke))))
  ;; draw text
  (call-next-method))

;; no override.
;(defmethod text-shape-calc-size ((cyl cylinder))
;  (call-next-method))

;; no override.
;(defmethod text-shape-paragraph-area ((cyl cylinder))
;  (call-next-method))

;;------------------------------------------------------------------------------
;;
;; macro cylinder
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:cylinder
 |#
(defmacro cylinder (center width height text
						 &key depth font align valign fill stroke link rotate layer id filter contents)
  (let ((code `(register-entity (make-instance 'cylinder
											   :center ,center :depth ,depth
											   :width ,width :height ,height
											   :text ,text :font ,font
											   :align ,align :valign ,valign
											   :fill ,fill :stroke ,stroke :link ,link 
											   :rotate ,rotate :layer ,layer
											   :filter ,filter :id ,id))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (diagram:shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))
