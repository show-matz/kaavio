#|
#|ASD|#				(:file "group"                     :depends-on ("cl-diagram"
#|ASD|#																"canvas"
#|ASD|#																"shape"
#|ASD|#																"rectangle"
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
;;(defmethod shape-canvas ((grp group)) ...)

(defmethod entity-composition-p ((grp group))
  t)  

(defmethod get-cc-point ((grp group) x y)
  (get-rectangle-cc-point (group-x      grp)
						  (group-y      grp)
						  (group-width  grp)
						  (group-height grp) x y))

(defmethod draw-entity ((grp group) writer)
	(pre-draw   grp writer)
	(draw-group grp writer)
	(post-draw  grp writer)
  nil)
					  


#|
#|EXPORT|#				:draw-group-frame
 |#
(defun draw-group-frame (grp writer)
  (let ((canvas (shape-canvas grp))
		(*default-stroke* (make-stroke :color :blue :dasharray '(3 3))))
	(declare (special canvas))
	(macrolet ((register-entity (entity)
				 (let ((g-entity (gensym "ENTITY")))
				   `(let ((,g-entity ,entity))
					  (setf (entity-canvas ,g-entity) canvas)
					  (check ,g-entity canvas nil)
					  (draw-entity ,g-entity  writer)))))
	  (with-slots (top bottom left right) canvas
		(rectangle (/ (- right  left) 2)
				   (/ (- bottom  top) 2)
				   (- right left)
				   (- bottom top) :fill :none)))))

;x y m-width m-height :fill :none)
;			(let ((x (- m-x (class:member canvas :left)))
;				  (y (- m-y (class:member canvas :top))))
;			  (rectangle x y m-width m-height :fill :none)
;			  (line `(,(- x 50) ,y ,(+ x 50) ,y))
;			  (line `(,x ,(- y 50) ,x ,(+ y 50)))))))))


