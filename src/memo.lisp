#|
#|ASD|#				(:file "memo"                      :depends-on ("cl-diagram"
#|ASD|#																"arc"
#|ASD|#																"polygon"
#|ASD|#																"filter"
#|ASD|#																"text-shape"))
#|EXPORT|#				;memo.lisp
 |#

(in-package :cl-diagram)

#|
#|EXPORT|#				:*default-memo-dog-ear*
#|EXPORT|#				:*default-memo-align*
#|EXPORT|#				:*default-memo-valign*
#|EXPORT|#				:*default-memo-margin*
#|EXPORT|#				:*default-memo-filter*
 |#
(defparameter *default-memo-dog-ear*  20)
(defparameter *default-memo-align*    :center)
(defparameter *default-memo-valign*   :center)
(defparameter *default-memo-margin*   10)
(defparameter *default-memo-filter*   nil)


(defun memo-get-points1 (w h e)
  `((0         0)
	(0        ,h)
	(,(- w e) ,h)
	(,w       ,(- h (/ e 2)))
	(,w        0)
	(0         0)))

(defun memo-get-points2 (w h e)
  `((,(- w e)       ,h)
	(,w             ,(- h (/ e 2)))
	(,(- w (/ e 1.5)) ,(- h (/ e 1.5)))
	(,(- w e)       ,h)))

;;------------------------------------------------------------------------------
;;
;; class memo
;;
;;------------------------------------------------------------------------------
(defclass memo (text-shape)
  ((dog-ear	:initform nil :initarg :dog-ear)  ; number
   (filter	:initform nil :initarg :filter))) ; (or nil keyword)
  
(defmethod initialize-instance :after ((obj memo) &rest initargs)
  (declare (ignore initargs))
  (with-slots (dog-ear filter) obj
	(setf dog-ear (or dog-ear *default-memo-dog-ear*))
	(setf filter  (if (eq filter :none)
					  nil
					  (or filter *default-memo-filter* *default-shape-filter*))))
  obj)
   
(defmethod check ((obj memo) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (dog-ear filter) obj
	(check-member dog-ear   :nullable nil :types number)
	(check-member filter    :nullable   t :types keyword))
  nil)

;; override of group::draw-group
(defmethod draw-group ((obj memo) writer)
  (let* ((canvas (group-get-canvas obj))
		 (width  (canvas-width  canvas))
		 (height (canvas-height canvas)))
	(macrolet ((register-entity (entity)
				 `(check-and-draw-local-entity ,entity canvas writer)))
	  (with-slots (dog-ear fill stroke filter) obj
		;; draw
		(polygon (memo-get-points1 width height dog-ear)
				 :fill fill :stroke stroke :filter filter)
		(polygon (memo-get-points2 width height dog-ear)
				 :fill (make-fill :base fill
								  :color (colormap-more-dark (slot-value fill 'color)))
				 :stroke (make-stroke :linejoin :round :base stroke)))))
  ;; draw text
  (call-next-method))

;; no override.
;(defmethod text-shape-calc-size ((obj memo))
;  (call-next-method))

;; no override.
;(defmethod text-shape-paragraph-area ((obj memo))
;  (call-next-method))

;;------------------------------------------------------------------------------
;;
;; macro memo
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:memo
 |#
(defmacro memo (center text &key dog-ear width height align valign
								 font fill stroke margin link rotate layer id filter contents)
  (let ((code `(register-entity (make-instance 'memo
											   :dog-ear ,dog-ear
											   :center ,center
											   :width ,width :height ,height
											   :text ,text :font ,font
											   :align  (or ,align  *default-memo-align*)
											   :valign (or ,valign *default-memo-valign*)
											   :margin (or ,margin *default-memo-margin*)
											   :fill ,fill :stroke ,stroke
											   :link ,link :rotate ,rotate
											   :filter ,filter :layer ,layer :id ,id))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (diagram:shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))

