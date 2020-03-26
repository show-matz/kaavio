#|
#|ASD|#				(:file "rectangle"                 :depends-on ("cl-diagram"
#|ASD|#																"constants"
#|ASD|#																"mathutil"
#|ASD|#																"canvas"
#|ASD|#																"point"
#|ASD|#																"shape"
#|ASD|#																"stroke-info"
#|ASD|#																"link-info"
#|ASD|#																"writer"))
#|EXPORT|#				;rectangle.lisp
 |#


(in-package :cl-diagram)

;-------------------------------------------------------------------------------
;
; utility functions
;
;-------------------------------------------------------------------------------
#|
#|EXPORT|#				:get-rectangle-cc-point
 |#
(defun get-rectangle-cc-point (cx cy width height px py)
  ;;(format t "cx=~A, cy=~A, width=~A, height=~A, px=~A, py=~A.~%" cx cy width height px py)
  (let* ((w/2   (/ width  2))
		 (h/2   (/ height 2))
		 (len1  (math/len4 cx cy (+ cx w/2) (+ cy h/2)))    ; length between center to corner.
		 (len2  (math/len4 cx cy px py))
		 (c-sin (/ h/2 len1))
		 (p-sin (/ (- py cy) len2))
		 (p-cos (/ (- px cx) len2)))
	;;(format t "c-sin=~A, p-sin=~A, p-cos=~A.~%" c-sin p-sin p-cos)
	(cond
	  ((< 0 c-sin p-sin)	 (make-point (+ cx (* (/ p-cos p-sin) h/2)) (+ cy h/2)))	;; bottom line
	  ((< p-sin (- c-sin) 0) (make-point (- cx (* (/ p-cos p-sin) h/2)) (- cy h/2)))	;;  upper line
	  ((< cx px)			 (make-point (+ cx w/2) (+ cy (* (/ p-sin p-cos) w/2))))	;;  right line
	  (t					 (make-point (- cx w/2) (- cy (* (/ p-sin p-cos) w/2)))))))	;;   left line


;-------------------------------------------------------------------------------
;
; shape rectangle
;
;-------------------------------------------------------------------------------
(defclass rectangle (shape)
  ((center-x	;:type     number
				:initform 0
				:initarg  :center-x
				:accessor shape-center)
   (center-y	;:type     number
				:initform 0
				:initarg  :center-y
				:accessor shape-middle)
   (width		;:type     number
				:initform 0
				:initarg  :width
				:accessor shape-width)
   (height		;:type     number
				:initform 0
				:initarg  :height
				:accessor shape-height)
   (rx			;:type     number
				:initform nil
				:initarg  :rx
				:accessor rectangle-rx)
   (ry			;:type     number
				:initform nil
				:initarg  :ry
				:accessor rectangle-ry)
   (fill		;:type     (or nil fill-info)
				:initform nil
				:initarg  :fill
				:accessor rectangle-fill)
   (stroke		;:type     (or nil link-info)
				:initform nil
				:initarg  :stroke
				:accessor rectangle-stroke)))


(defmethod initialize-instance :after ((rct rectangle) &rest initargs)
  (declare (ignore initargs))
  (with-slots (rx ry fill stroke) rct
	(unless rx
	  (setf rx *default-rectangle-rx*))
	(unless ry
	  (setf ry *default-rectangle-ry*))
	(setf fill (if (null fill)
				   *default-fill*
				   (make-fill fill)))
	(setf stroke (if (null stroke)
					 *default-stroke*
					 (make-stroke stroke))))
  rct)

(defmethod check ((rct rectangle) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (check-member (center-x (shape-center rct)) :nullable nil :types number)
  (check-member (center-y (shape-middle rct)) :nullable nil :types number)
  (check-member (width    (shape-width  rct)) :nullable nil :types number)
  (check-member (height   (shape-height rct)) :nullable nil :types number)
  (check-member (rx       (rectangle-rx rct)) :nullable   t :types number)
  (check-member (ry       (rectangle-ry rct)) :nullable   t :types number)
  (check-object (fill   (rectangle-fill   rct)) canvas dict :nullable t :class   fill-info)
  (check-object (stroke (rectangle-stroke rct)) canvas dict :nullable t :class stroke-info)
  (incf (shape-center rct) (canvas-left canvas))
  (incf (shape-middle rct) (canvas-top  canvas))
  nil)

(defmethod get-cc-point ((shp rectangle) x y)
  (get-rectangle-cc-point (shape-center shp)
						  (shape-middle shp)
						  (shape-width  shp)
						  (shape-height shp) x y))

;;MEMO : use impelementation of shape...
;;(defmethod shape-canvas ((shp rectangle)) ...)

(defmethod draw-entity ((rct rectangle) writer)
  (let ((cls  (shape-class rct))
		(id   (and (not (entity-composition-p rct))
				   (entity-id rct))))
	(pre-draw rct writer)
	(writer-write writer
				  "<rect "
				  (write-when id "id='" it "' ")
				  "x='" (shape-left rct) "' "
				  "y='" (shape-top  rct) "' "
				  "width='"  (shape-width  rct) "' "
				  "height='" (shape-height rct) "' "
				  (write-when (rectangle-rx rct) "rx='" it "' ")
				  (write-when (rectangle-ry rct) "ry='" it "' ")
				  (write-when cls "class='" it "' ")
				  (unless cls
					(let ((fill (rectangle-fill   rct)))
					  (when fill
						(to-property-strings fill))))
				  (unless cls
					(let ((strk (rectangle-stroke rct)))
					  (when strk
						(to-property-strings strk))))
				  "/>")
	(post-draw rct writer))
  nil)
  

#|
#|EXPORT|#				:rectangle
 |#
(defmacro rectangle (x y width height
					 &key rx ry class fill stroke link layer id contents)
  (let ((code `(register-entity (make-instance 'diagram:rectangle
											   :center-x ,x :center-y ,y
											   :width ,width :height ,height
											   :rx ,rx :ry ,ry :class ,class
											   :fill ,fill :stroke ,stroke
											   :link ,link :layer ,layer :id ,id))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (diagram:shape-canvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))

