#|
#|ASD|#				(:file "arc"                       :depends-on ("cl-diagram"
#|ASD|#																"mathutil"
#|ASD|#																"path"))
#|EXPORT|#				;arc.lisp
 |#

(in-package :cl-diagram)

;-------------------------------------------------------------------------------
;
; class arc
;
;-------------------------------------------------------------------------------
(defclass arc (path)
  ((center-x	:initform 0 :initarg :center-x)		; number
   (center-y	:initform 0 :initarg :center-y)		; number
   (radius		:initform 0 :initarg :radius)		; number
   (degree1		:initform 0 :initarg :degree1)		; number
   (degree2		:initform 0 :initarg :degree2)))	; number

;;MEMO : no implementation
;;(defmethod initialize-instance :after ((ent arc) &rest initargs)...)


(defun arc-calculate-data (ent)
  (with-slots (center-x center-y
						radius degree1 degree2) ent
	(let ((start-x (+ center-x (* radius (math/cos1 degree1))))
		  (start-y (+ center-y (* radius (math/sin1 degree1))))
		  (end-x   (+ center-x (* radius (math/cos1 degree2))))
		  (end-y   (+ center-y (* radius (math/sin1 degree2))))
		  (x-axis-rotation 0)    ; fixed.
		  (sweep-flag      1)    ; fixed.
		  (large-arc-flag  (if (<= 180 (let ((tmp (- degree2 degree1)))
										 (if (minusp tmp) (+ 360 tmp) tmp))) 1 0)))
	  `(:absolute :move-to ,start-x ,start-y
				  :arc-to  ,radius ,radius ,x-axis-rotation
						   ,large-arc-flag ,sweep-flag ,end-x ,end-y))))


(defmethod check ((ent arc) canvas dict)
  (declare (ignorable dict))
  (with-slots (center-x center-y radius degree1 degree2) ent
	(check-member center-x :nullable nil :types number)
	(check-member center-y :nullable nil :types number)
	(check-member radius   :nullable nil :types number)
	(check-member degree1  :nullable nil :types number)
	(check-member degree2  :nullable nil :types number))
  (setf (slot-value ent 'data) (arc-calculate-data ent))
  ;; this method must call super class' one.
  (call-next-method))


;; this class NOT override draw-entity ( use path class one ).
;;(defmethod draw-entity ((ent arc) writer)...)


#|
#|EXPORT|#				:arc
 |#
(defmacro arc (x y radius degree1 degree2 &key class stroke layer id)
  `(register-entity (make-instance 'diagram:arc
								   :center-x ,x :center-y ,y
								   :radius ,radius :degree1 ,degree1 :degree2 ,degree2
								   :class ,class :stroke ,stroke :layer ,layer :id ,id)))

