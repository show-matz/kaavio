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
   (pt1	    		:initform nil)								; point
   (pt2	    		:initform nil)								; point
   (rx	    		:initform   0 :initarg :rx)					; number
   (ry    			:initform   0 :initarg :ry)					; number
   (x-axis-rotation	:initform   0 :initarg :x-axis-rotation)	; number
   (end1			:initform nil :initarg :end1)				; keyword
   (end2			:initform nil :initarg :end2)				; keyword
   (degree1			:initform   0 :initarg :degree1)			; number
   (degree2			:initform   0 :initarg :degree2)			; number
   (debug			:initform nil :initarg :debug)))			; (or nil t keyword)

(defmethod initialize-instance :after ((ent arc) &rest initargs)
  (declare (ignore initargs))
  (with-slots (end1 end2 debug) ent
	(setf end1   (make-endmark (or end1   *default-endmark-1*)))
	(setf end2   (make-endmark (or end2   *default-endmark-2*)))
	(when debug
	  (setf debug (if (keywordp debug) debug :red))))
  ent)


(defun arc-calculate-data (center rx ry x-axis-rotation degree1 degree2)
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
	  (values x0 y0 x1 y1 large-arc sweep-flag))))

(defun arc-draw-base-ellipse (ent writer)
  (with-slots (center rx ry x-axis-rotation debug) ent
	(when x-axis-rotation
	  (writer-write writer "<g transform='rotate(" x-axis-rotation ","
								(point-x center) "," (point-y center) ")'>"))
	(writer-write writer
				  "<ellipse "
				  "cx='" (point-x center) "' "
				  "cy='" (point-y center) "' "
				  "rx='" rx "' "
				  "ry='" ry "' "
				  "fill='none' "
				  (to-property-strings (make-stroke :color debug :dasharray '(1 4)))
				  "/>")
	(when x-axis-rotation
	  (writer-write writer "</g>"))))

(defun arc-draw-endmarks (ent writer)
  (with-slots (pt1 pt2 rx ry x-axis-rotation
				  degree1 degree2 end1 end2 stroke debug) ent
	(let ((sin-d1 (math/sin1 x-axis-rotation))
		  (cos-d1 (math/cos1 x-axis-rotation)))
	  ;; 原点中心、回転なしの正規化されたベース楕円で始点 (k1 m1)、終点 (k2 m2) を再計算
	  (multiple-value-bind (k1 m1 k2 m2)
		  (arc-calculate-data '(0 0) rx ry 0 degree1 degree2)
		(when end1
		  ;;正規化されたベース楕円における (k1 m1) を通る接線の sin/cos を求める
		  (multiple-value-bind (sin-d2 cos-d2)
			  (cond
				((zerop k1) (values 0 (if (< 0 m1) 1 -1)))
				((zerop m1) (values (if (< 0 k1) -1 1) 0))
				(t (let ((x (/ (* rx rx) k1))
						 (y (/ (* ry ry) m1)))
					 (if (< 0 (* k1 m1))
						 (values (math/sin2 `(0 ,y) `(,x 0))
								 (math/cos2 `(0 ,y) `(,x 0)))
						 (values (math/sin2 `(,x 0) `(0 ,y))
								 (math/cos2 `(,x 0) `(0 ,y)))))))
			;; ベース楕円の回転角と合成して sin θ と cos θ（pt1 における接線の sin/cos）を求める
			(let* ((sin-theta (+ (* sin-d1 cos-d2) (* cos-d1 sin-d2)))
				   (cos-theta (- (* cos-d1 cos-d2) (* sin-d1 sin-d2)))
				   (pw (xy+ pt1 (- (* 20 cos-theta)) (- (* 20 sin-theta)))))
			  (draw-endmark end1 (cons pw pt1) stroke writer))))
		(when end2
		  ;;正規化されたベース楕円における (k2 m2) を通る接線の sin/cos を求める
		  (multiple-value-bind (sin-d2 cos-d2)
			  (cond
				((zerop k2) (values 0 (if (< 0 m2) -1 1)))
				((zerop m2) (values (if (< 0 k2) 1 -1) 0))
				(t (let ((x (/ (* rx rx) k2))
						 (y (/ (* ry ry) m2)))
					 (if (< 0 (* k2 m2))
						 (values (math/sin2 `(,x 0) `(0 ,y))
								 (math/cos2 `(,x 0) `(0 ,y)))
						 (values (math/sin2 `(0 ,y) `(,x 0))
								 (math/cos2 `(0 ,y) `(,x 0)))))))
			;; ベース楕円の回転角と合成して sin θ と cos θ（pt1 における接線の sin/cos）を求める
			(let* ((sin-theta (+ (* sin-d1 cos-d2) (* cos-d1 sin-d2)))
				   (cos-theta (- (* cos-d1 cos-d2) (* sin-d1 sin-d2)))
				   (pw (xy+ pt2 (- (* 20 cos-theta)) (- (* 20 sin-theta)))))
			  (draw-endmark end2 (cons pw pt2) stroke writer))))))))

(defun arc-draw-controls (ent writer)
  (with-slots (center pt1 pt2 debug) ent
    (let ((stroke (make-stroke :color debug :dasharray '(4 3)))
		  (fill   (make-fill   :color debug)))
	  (labels ((draw-ctrl-point (pt)
				 (writer-write writer
							   "<circle "
							   "cx='" (point-x pt) "' "
							   "cy='" (point-y pt) "' "
							   "r='3' stroke='none' "
							   (to-property-strings fill)
							   "/>"))
			   (draw-line (pt1 pt2)
				 (writer-write writer
							   "<polyline fill='none' "
							   (to-property-strings stroke)
							   "points='"
							   (with-output-to-string (st)
								 (format st "~A,~A ~A,~A"
										 (coerce (point-x pt1) 'single-float)
										 (coerce (point-y pt1) 'single-float)
										 (coerce (point-x pt2) 'single-float)
										 (coerce (point-y pt2) 'single-float)))
							   "' />")))
		(draw-line center pt1)
		(draw-line center pt2)
		(draw-ctrl-point  pt1)
		(draw-ctrl-point  pt2)
		(draw-ctrl-point  center)))))

(defmethod check ((ent arc) canvas dict)
  (declare (ignorable dict))
  (with-slots (center pt1 pt2 rx ry x-axis-rotation
					  degree1 degree2 end1 end2 debug) ent
	(check-member rx              :nullable nil :types number)
	(check-member ry              :nullable nil :types number)
	(check-member x-axis-rotation :nullable nil :types number)
	(check-member degree1         :nullable nil :types number)
	(check-member degree2         :nullable nil :types number)
	(check-object end1 canvas dict :nullable  t :class endmark-info)
	(check-object end2 canvas dict :nullable  t :class endmark-info)
	(check-member debug           :nullable   t :types keyword)
	(when end1 (check end1 canvas dict))
	(when end2 (check end2 canvas dict))
	(multiple-value-bind (x0 y0 x1 y1 large-arc sweep-flag)
						 (arc-calculate-data center rx ry x-axis-rotation degree1 degree2)
	  (let ((cc (canvas-topleft canvas)))
		(setf pt1 (make-point (- x0 (point-x cc)) (- y0 (point-y cc))))
		(setf pt2 (make-point (- x1 (point-x cc)) (- y1 (point-y cc))))
		(setf (slot-value ent 'data) `(:absolute
									   (:move-to ,pt1)
									   (:arc-to ,rx ,ry ,x-axis-rotation
												,large-arc ,sweep-flag ,pt2)))))
	(setf pt1 (canvas-fix-point canvas pt1))
	(setf pt2 (canvas-fix-point canvas pt2)))
  ;; this method must call super class' one.
  (call-next-method))


(defmethod entity-composition-p ((ent arc))
  (or (slot-value ent 'end1)
	  (slot-value ent 'end2)))

(defmethod draw-entity ((ent arc) writer)
  (with-slots (end1 end2 debug) ent
	;; debug mode ならベースになる楕円を描画
	(when debug
	  (arc-draw-base-ellipse ent writer))
	;; arc 本体の描画
	(call-next-method)
	;; endmark 処理 : internal doc の「円弧の端点における傾きの計算方法」参照
	(when (or end1 end2)
	  (arc-draw-endmarks ent writer))
	;; debug mode なら中心と始点、終点、およびそれらの間の線を描く
	(when debug
	  (arc-draw-controls ent writer))))


(defmethod attribute-center ((ent arc))
  (slot-value ent 'center))

(defmethod attribute-end1 ((ent arc))
  (slot-value ent 'pt1))

(defmethod attribute-end2 ((ent arc))
  (slot-value ent 'pt2))


;;------------------------------------------------------------------------------
;;
;; macro arc
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:arc
 |#
(defmacro arc (center rx ry x-axis-rotation
					  degree1 degree2 &key stroke end1 end2 layer filter id debug)
  `(register-entity (make-instance 'kaavio:arc
								   :center ,center :rx ,rx :ry ,ry
								   :x-axis-rotation ,x-axis-rotation
								   :degree1 ,degree1 :degree2 ,degree2
								   :end1 ,end1 :end2 ,end2
								   :stroke ,stroke :fill :none :layer ,layer
								   :filter ,filter :id ,id :debug ,debug)))

