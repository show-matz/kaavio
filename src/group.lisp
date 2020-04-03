#|
#|ASD|#				(:file "group"                     :depends-on ("cl-diagram"
#|ASD|#																"canvas"
#|ASD|#																"shape"
#|ASD|#																"writer"))
#|EXPORT|#				;group.lisp
 |#


(in-package :cl-diagram)

;-------------------------------------------------------------------------------
;
; abstract shape group
;
;-------------------------------------------------------------------------------
#|
#|EXPORT|#				:group
#|EXPORT|#				:group-get-canvas
#|EXPORT|#				:draw-group
 |#
(defclass group (shape)
  ((x		;:type     number
			:initform 0
			:initarg  :center-x
			:accessor group-x)
   (y		;:type     number
			:initform 0
			:initarg  :center-y
			:accessor group-y)
   (width	;:type     number
			:initform 0
			:initarg  :width
			:accessor group-width)
   (height	;:type     number
			:initform 0
			:initarg  :height
			:accessor group-height)))

(defgeneric group-get-canvas (grp))
(defgeneric draw-group (grp writer))



(defmethod initialize-instance :after ((grp group) &rest initargs)
  (declare (ignore initargs))
  ;; currently do nothing...
  grp)

(defmethod check ((grp group) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (check-member (x      (group-x  grp)) :nullable nil :types number)
  (check-member (y      (group-y  grp)) :nullable nil :types number)
  (check-member (width  (group-x  grp)) :nullable nil :types number)
  (check-member (height (group-y  grp)) :nullable nil :types number)
  (incf (group-x grp) (canvas-left canvas))
  (incf (group-y grp) (canvas-top  canvas))
  nil)

(defmethod shape-width ((grp group))
  (group-width grp))

(defmethod shape-height ((grp group))
  (group-height grp))

(defmethod shape-top ((grp group))
  (- (group-y grp)
	 (/ (group-height grp) 2)))

(defmethod shape-middle ((grp group))
  (group-y grp))

(defmethod shape-bottom ((grp group))
  (+ (group-y grp)
	 (/ (group-height grp) 2)))

(defmethod shape-left ((grp group))
  (- (group-x grp)
	 (/ (group-width grp) 2)))

(defmethod shape-center ((grp group))
  (group-x grp))

(defmethod shape-right ((grp group))
  (+ (group-x grp)
	 (/ (group-width grp) 2)))

;;MEMO : use impelementation of shape...
;;(defmethod shape-connect-point ((grp group) type arg) ...)
  
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
  (make-canvas (shape-top    grp)
			   (shape-bottom grp)
			   (shape-left   grp)
			   (shape-right  grp)))


#|
#|EXPORT|#				:draw-canvas-frame
 |#
(defun draw-canvas-frame (canvas writer &key (color :blue))
  (let ((*default-fill*   (make-fill   :color color :opacity 0.2))
		(*default-stroke* (make-stroke :color color :dasharray '(2 2))))
	(macrolet ((register-entity (entity)
				 `(check-and-draw-local-entity ,entity canvas writer)))
	  (with-canvas (top bottom left right) canvas
		(rectangle (/ (- right  left) 2)
				   (/ (- bottom  top) 2)
				   (- right left)
				   (- bottom top))))))

;x y m-width m-height :fill :none)
;			(let ((x (- m-x (class:member canvas :left)))
;				  (y (- m-y (class:member canvas :top))))
;			  (rectangle x y m-width m-height :fill :none)
;			  (line `(,(- x 50) ,y ,(+ x 50) ,y))
;			  (line `(,x ,(- y 50) ,x ,(+ y 50)))))))))


