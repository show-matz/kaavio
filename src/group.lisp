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
  ((x		:initform 0 :initarg :center-x)	; number
   (y		:initform 0 :initarg :center-y)	; number
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
  (with-slots (x y width height) grp
	(check-member x      :nullable nil :types number)
	(check-member y      :nullable nil :types number)
	(check-member width  :nullable nil :types number)
	(check-member height :nullable nil :types number)
	(incf x (canvas-left canvas))
	(incf y (canvas-top  canvas)))
  nil)

(defmethod shape-width ((grp group))
  (slot-value grp 'width))

(defmethod shape-height ((grp group))
  (slot-value grp 'height))

(defmethod shape-top ((grp group))
  (- (slot-value grp 'y)
	 (/ (slot-value grp 'height) 2)))

(defmethod shape-middle ((grp group))
  (slot-value grp 'y))

(defmethod shape-bottom ((grp group))
  (+ (slot-value grp 'y)
	 (/ (slot-value grp 'height) 2)))

(defmethod shape-left ((grp group))
  (- (slot-value grp 'x)
	 (/ (slot-value grp 'width) 2)))

(defmethod shape-center ((grp group))
  (slot-value grp 'x))

(defmethod shape-right ((grp group))
  (+ (slot-value grp 'x)
	 (/ (slot-value grp 'width) 2)))

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


