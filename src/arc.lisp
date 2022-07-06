#|
#|ASD|#				(:file "arc"                       :depends-on ("cl-diagram"
#|ASD|#																"point"
#|ASD|#																"canvas"
#|ASD|#																"mathutil"
#|ASD|#																"path"))
#|EXPORT|#				;arc.lisp
 |#

(in-package :cl-diagram)

;;------------------------------------------------------------------------------
;;
;; class arc
;;
;;------------------------------------------------------------------------------
(defclass arc (path)
  ((center		:initform nil :initarg :center)		; point
   (radius		:initform   0 :initarg :radius)		; number
   (degree1		:initform   0 :initarg :degree1)	; number
   (degree2		:initform   0 :initarg :degree2)))	; number

;;MEMO : no implementation
;;(defmethod initialize-instance :after ((ent arc) &rest initargs)...)


(defun arc-calculate-data (ent)
  (with-slots (center radius degree1 degree2) ent
	(let ((start (point/xy+ center (* radius (math/cos1 degree1))
								   (* radius (math/sin1 degree1))))
		  (end   (point/xy+ center (* radius (math/cos1 degree2))
								   (* radius (math/sin1 degree2))))
		  (x-axis-rotation 0)    ; fixed.
		  (sweep-flag      1)    ; fixed.
		  (large-arc-flag  (if (<= 180 (let ((tmp (- degree2 degree1)))
										 (if (minusp tmp) (+ 360 tmp) tmp))) 1 0)))
	  `(:absolute (:move-to ,start)
				  (:arc-to ,radius ,radius ,x-axis-rotation
						   ,large-arc-flag ,sweep-flag ,end)))))


(defmethod check ((ent arc) canvas dict)
  (declare (ignorable dict))
  (with-slots (center radius degree1 degree2) ent
	(check-member radius   :nullable nil :types number)
	(check-member degree1  :nullable nil :types number)
	(check-member degree2  :nullable nil :types number)
	(setf center (canvas-fix-point canvas center)))
  (setf (slot-value ent 'data) (arc-calculate-data ent))
  ;; this method must call super class' one.
  (call-next-method))


;; this class NOT override draw-entity ( use path class one ).
;;(defmethod draw-entity ((ent arc) writer)...)


;;------------------------------------------------------------------------------
;;
;; macro arc
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:arc
 |#
(defmacro arc (center radius degree1 degree2 &key stroke layer filter id)
  `(register-entity (make-instance 'diagram:arc
								   :center ,center :radius ,radius
								   :degree1 ,degree1 :degree2 ,degree2
								   :stroke ,stroke :fill :none
								   :layer ,layer :filter ,filter :id ,id)))

