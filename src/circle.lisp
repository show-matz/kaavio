#|
#|ASD|#				(:file "circle"                    :depends-on ("cl-diagram"
#|ASD|#																"constants"
#|ASD|#																"mathutil"
#|ASD|#																"canvas"
#|ASD|#																"point"
#|ASD|#																"shape"
#|ASD|#																"stroke-info"
#|ASD|#																"link-info"
#|ASD|#																"writer"))
#|EXPORT|#				;circle.lisp
 |#


(in-package :cl-diagram)

;-------------------------------------------------------------------------------
;
; utility functions
;
;-------------------------------------------------------------------------------
#|
#|EXPORT|#				:circle-connect-point
 |#
(defun circle-connect-point (cx cy radius type arg)
  (if (eq type :center)
	  (with-point (px py) arg
		(let ((x (* radius (math/cos4 px py cx cy)))
			  (y (* radius (math/sin4 px py cx cy))))
		  (make-point (- cx x) (- cy y))))
	  ;ToDo : 6jT47EsHawK : below is temporary code...
	  (let ((degree (ecase type
					  ((:right)  (+   0 (* arg 30)))
					  ((:bottom) (-  90 (* arg 30)))
					  ((:left)   (- 180 (* arg 30)))
					  ((:top)    (+ 270 (* arg 30))))))
		(when (< degree 0)
		  (incf degree 360))
		(let ((x (* radius (math/cos1 degree)))
			  (y (* radius (math/sin1 degree))))
		  (make-point (+ cx x) (+ cy y))))))


;-------------------------------------------------------------------------------
;
; shape circle
;
;-------------------------------------------------------------------------------
(defclass circle (shape)
  ((center-x	;:type     number
				:initform 0
				:initarg  :center-x
				:accessor shape-center)
   (center-y	;:type     number
				:initform 0
				:initarg  :center-y
				:accessor shape-middle)
   (radius		;:type     number
				:initform 0
				:initarg  :radius
				:accessor circle-radius)
   (fill		;:type     (or nil fill-info)
				:initform nil
				:initarg  :fill
				:accessor circle-fill)
   (stroke		;:type     (or nil stroke-info)
				:initform nil
				:initarg  :stroke
				:accessor circle-stroke)))

(defmethod initialize-instance :after ((ent circle) &rest initargs)
  (declare (ignore initargs))
  (with-slots (fill stroke) ent
	(setf fill (if (null fill)
				   *default-fill*
				   (make-fill fill)))
	(setf stroke (if (null stroke)
					 *default-stroke*
					 (make-stroke stroke))))
  ent)

(defmethod check ((shp circle) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (check-member (center-x (shape-center  shp)) :nullable nil :types number)
  (check-member (center-y (shape-middle  shp)) :nullable nil :types number)
  (check-member (radius   (circle-radius shp)) :nullable nil :types number)
  (check-object (fill     (circle-fill   shp)) canvas dict :nullable t :class   fill-info)
  (check-object (stroke   (circle-stroke shp)) canvas dict :nullable t :class stroke-info)
  (incf (shape-center shp) (canvas-left canvas))
  (incf (shape-middle shp) (canvas-top  canvas))
  nil)

(defmethod shape-width ((shp circle))
  (* 2 (circle-radius shp)))

(defmethod shape-height ((shp circle))
  (* 2 (circle-radius shp)))

(defmethod shape-top ((shp circle))
  (- (shape-middle shp) (circle-radius shp)))

(defmethod shape-bottom ((shp circle))
  (+ (shape-middle shp) (circle-radius shp)))

(defmethod shape-left ((shp circle))
  (- (shape-center shp) (circle-radius shp)))

(defmethod shape-right ((shp circle))
  (+ (shape-center shp) (circle-radius shp)))

(defmethod shape-connect-point ((shp circle) type arg)
  (circle-connect-point (shape-center  shp)
						(shape-middle  shp)
						(circle-radius shp) type arg))
  
;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((shp circle)) ...)

;;MEMO : use impelementation of shape...
;;(defmethod entity-composition-p ((shp circle)) ...)
  
(defmethod draw-entity ((shp circle) writer)
  (let ((cls  (shape-class shp))
		(id   (and (not (entity-composition-p shp))
				   (entity-id shp))))
	(pre-draw shp writer)
	(writer-write writer
				  "<circle "
				  (write-when id "id='" it "' ")
				  "cx='" (shape-center shp) "' "
				  "cy='" (shape-middle shp) "' "
				  "r='" (circle-radius shp) "' "
				  (write-when cls "class='" it "' ")
				  (unless cls
					(let ((fill (circle-fill   shp)))
					  (when fill
						(to-property-strings fill))))
				  (unless cls
					(let ((strk (circle-stroke shp)))
					  (when strk
						(to-property-strings strk))))
				  "/>")
	(post-draw shp writer))
  nil)


#|
#|EXPORT|#				:circle
 |#
(defmacro circle (x y radius
				  &key class fill stroke link layer id contents)
  (let ((code `(register-entity (make-instance 'diagram:circle
											   :center-x ,x :center-y ,y 
											   :radius ,radius :class ,class
											   :fill ,fill :stroke ,stroke
											   :link ,link :layer ,layer :id ,id))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (diagram:shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))

