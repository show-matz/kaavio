#|
#|ASD|#				(:file "arc"                       :depends-on ("kaavio"
#|ASD|#																"point"
#|ASD|#																"canvas"
#|ASD|#																"mathutil"
#|ASD|#																"path"))
#|EXPORT|#				;arc.lisp
 |#

(in-package :kaavio)

;;------------------------------------------------------------------------------
;;
;; class arc
;;
;;------------------------------------------------------------------------------
(defclass arc (path)
  ((center			:initform nil :initarg :center)				; point
   (rx	    		:initform   0 :initarg :rx)					; number
   (ry    			:initform   0 :initarg :ry)					; number
   (x-axis-rotation	:initform   0 :initarg :x-axis-rotation)	; number
   (degree1			:initform   0 :initarg :degree1)			; number
   (degree2			:initform   0 :initarg :degree2)))			; number

;;MEMO : no implementation
;;(defmethod initialize-instance :after ((ent arc) &rest initargs)...)


(defun arc-calculate-data (ent)
  (with-slots (center rx ry x-axis-rotation degree1 degree2) ent
	(let ((cx (point-x center))
		  (cy (point-y center))
		  (delta (- degree2 degree1)))
	  (when (< delta 0)
		(incf delta 360))
	  (let ((x0 (+ cx (* rx    (math/cos1 x-axis-rotation)  (math/cos1 degree1))
					  (* ry (- (math/sin1 x-axis-rotation)) (math/sin1 degree1))))
			(y0 (+ cy (* rx    (math/sin1 x-axis-rotation)  (math/cos1 degree1))
					  (* ry    (math/cos1 x-axis-rotation)  (math/sin1 degree1))))
			(x1 (+ cx (* rx    (math/cos1 x-axis-rotation)  (math/cos1 degree2))
					  (* ry (- (math/sin1 x-axis-rotation)) (math/sin1 degree2))))
			(y1 (+ cy (* rx    (math/sin1 x-axis-rotation)  (math/cos1 degree2))
					  (* ry    (math/cos1 x-axis-rotation)  (math/sin1 degree2))))
			(sweep-flag 1)	;; always 1
			(large-arc  (if (< 180 delta) 1 0)))
		(list x0 y0 x1 y1 rx ry x-axis-rotation large-arc sweep-flag)))))


(defmethod check ((ent arc) canvas dict)
  (declare (ignorable dict))
  (with-slots (center rx ry x-axis-rotation degree1 degree2) ent
	(check-member rx              :nullable nil :types number)
	(check-member ry              :nullable nil :types number)
	(check-member x-axis-rotation :nullable nil :types number)
	(check-member degree1         :nullable nil :types number)
	(check-member degree2         :nullable nil :types number))
  (destructuring-bind (x0 y0 x1 y1 rx ry
					   x-axis-rotation large-arc sweep-flag) (arc-calculate-data ent)
	(let* ((cc (canvas-topleft canvas))
		   (pt0 (make-point (- x0 (point-x cc)) (- y0 (point-y cc))))
		   (pt1 (make-point (- x1 (point-x cc)) (- y1 (point-y cc)))))
	  (setf (slot-value ent 'data) `(:absolute
									 (:move-to ,pt0)
									 (:arc-to ,rx ,ry ,x-axis-rotation
											  ,large-arc ,sweep-flag ,pt1)))))
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
(defmacro arc (center rx ry x-axis-rotation degree1 degree2 &key stroke layer filter id)
  `(register-entity (make-instance 'kaavio:arc
								   :center ,center :rx ,rx :ry ,ry
								   :x-axis-rotation ,x-axis-rotation
								   :degree1 ,degree1 :degree2 ,degree2
								   :stroke ,stroke :fill :none
								   :layer ,layer :filter ,filter :id ,id)))

