#|
#|ASD|#				(:file "cylinder"                  :depends-on ("cl-diagram"
#|ASD|#																"path"
#|ASD|#																"filter"
#|ASD|#																"text-shape"))
#|EXPORT|#				;cylinder.lisp
 |#

(in-package :cl-diagram)

#|
#|EXPORT|#				:*default-cylinder-align*
#|EXPORT|#				:*default-cylinder-valign*
#|EXPORT|#				:*default-cylinder-margin*
#|EXPORT|#				:*default-cylinder-font*
#|EXPORT|#				:*default-cylinder-fill*
#|EXPORT|#				:*default-cylinder-stroke*
#|EXPORT|#				:*default-cylinder-filter*
 |#
(defparameter *default-cylinder-align*    :center)
(defparameter *default-cylinder-valign*   :center)
(defparameter *default-cylinder-margin*        10)
(defparameter *default-cylinder-font*         nil)
(defparameter *default-cylinder-fill*         nil)
(defparameter *default-cylinder-stroke*       nil)
(defparameter *default-cylinder-filter*       nil)

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
	(setf filter (if (eq filter :none)
					 nil
					 (or filter *default-cylinder-filter* *default-shape-filter*))))
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
						 &key depth font align valign margin
							  fill stroke link rotate layer id filter contents)
  (let ((code `(register-entity (make-instance 'cylinder
											   :center ,center :depth ,depth
											   :width ,width :height ,height
											   :text ,text
 											   :align  (or ,align  *default-cylinder-align*)
 											   :valign (or ,valign *default-cylinder-valign*)
 											   :margin (or ,margin *default-cylinder-margin*)
											   :font   (or ,font   *default-cylinder-font*)
											   :fill   (or ,fill   *default-cylinder-fill*)
											   :stroke (or ,stroke *default-cylinder-stroke*)
											   :link ,link  :rotate ,rotate
											   :filter ,filter :layer ,layer :id ,id))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (diagram:shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))
