#|
#|ASD|#				(:file "group"                     :depends-on ("cl-diagram"
#|ASD|#																"canvas"
#|ASD|#																"shape"
#|ASD|#																"writer"))
#|EXPORT|#				;group.lisp
 |#


(in-package :cl-diagram)

;;------------------------------------------------------------------------------
;;
;; abstract class group
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:group
#|EXPORT|#				:group-get-canvas
#|EXPORT|#				:draw-group
 |#
(defclass group (shape)
  ((center	:initform 0 :initarg :center)	; number
   (width	:initform 0 :initarg :width)	; number
   (height	:initform 0 :initarg :height)))	; number

(defgeneric group-get-canvas (grp))
(defgeneric draw-group (grp writer))



(defmethod initialize-instance :after ((grp group) &rest initargs)
  (declare (ignore initargs))
  ;; currently do nothing...
  grp)

(defmethod check ((grp group) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (center width height) grp
	(check-member width  :nullable nil :types number)
	(check-member height :nullable nil :types number)
	(setf center (canvas-fix-point canvas center)))
  nil)

(defmethod shape-width ((grp group))
  (slot-value grp 'width))

(defmethod shape-height ((grp group))
  (slot-value grp 'height))

(defmethod shape-center ((grp group))
  (slot-value grp 'center))

;;MEMO : use impelementation of shape...
;;(defmethod shape-connect-point ((grp group) type1 type2 arg) ...)
  
;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((grp group)) ...)

(defmethod entity-composition-p ((grp group))
  t)  

(defmethod draw-entity ((grp group) writer)
	(pre-draw   grp writer)
	(draw-group grp writer)
	(post-draw  grp writer)
  nil)
					  

(defmethod group-get-canvas ((grp group))
  (make-canvas (shape-topleft grp)
			   (shape-width   grp)
			   (shape-height  grp)))


#|
#|EXPORT|#				:draw-canvas-frame
 |#
(defun draw-canvas-frame (canvas writer &key (color :blue))
  (let ((*default-fill*   (make-fill   :color color :opacity 0.2))
		(*default-stroke* (make-stroke :color color :dasharray '(2 2))))
	(macrolet ((register-entity (entity)
				 `(check-and-draw-local-entity ,entity canvas writer)))
	  (with-canvas (topleft width height) canvas
		(rectangle (point/xy+ topleft
							  (/ width 2)
							  (/ height 2)) width height)))))

;x y m-width m-height :fill :none)
;			(let ((x (- m-x (class:member canvas :left)))
;				  (y (- m-y (class:member canvas :top))))
;			  (rectangle x y m-width m-height :fill :none)
;			  (line `(,(- x 50) ,y ,(+ x 50) ,y))
;			  (line `(,x ,(- y 50) ,x ,(+ y 50)))))))))


