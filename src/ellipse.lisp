#|
#|ASD|#				(:file "ellipse"                   :depends-on ("cl-diagram"
#|ASD|#																"constants"
#|ASD|#																"canvas"
#|ASD|#																"point"
#|ASD|#																"shape"
#|ASD|#																"stroke-info"
#|ASD|#																"link-info"
#|ASD|#																"writer"))
#|EXPORT|#				;ellipse.lisp
 |#


(in-package :cl-diagram)

;-------------------------------------------------------------------------------
;
; utility functions
;
;-------------------------------------------------------------------------------
#|
#|EXPORT|#				:ellipse-connect-point
 |#
(defun ellipse-connect-point-C (cx cy rx ry pt)
  (with-point (pt-x pt-y) pt
	(if (= cx pt-x)
		;;■縦方向の直線になる場合は特別扱いする
		(if (< pt-y cy)
			(make-point cx (- cy ry))
			(make-point cx (+ cy ry)))
		;;■まず、point の座標から center を引くことで、楕円の中心を原点として処理を簡略化する。
		;;■上記により、線分の式は y = mx とできるので、m = Py / Px
		(let* ((px (- pt-x cx))
			   (py (- pt-y cy))
			   (m  (/ py px)))
		  ;;■楕円は x^2 / rx^2 + y^2 / ry^2 = 1 とすることができる。
		  ;;■これに y = mx を代入すると、x^2 / rx^2 + (mx)^2 / ry^2 = 1
		  ;;■これを整理して、x^2 = (rx^2 * ry^2) / ( ry^2 + (mrx)^2 )
		  (let ((x (sqrt (/ (* rx rx ry ry) (+ (* ry ry) (* m m rx rx))))))
			(when (and (not (< px x 0)) (not (< 0 x px)))
			  (setf x (* x -1)))
			(make-point (+ x cx) (+ (* m x) cy)))))))

(defun ellipse-connect-point-TB-impl (cx cy rx ry x)
  (declare (ignore cy))
  ;;■まず、x から cx を引くことで、楕円の中心を原点として処理を簡略化する。
  (let ((px (- x cx)))
	;;■楕円は x^2 / rx^2 + y^2 / ry^2 = 1 とすることができる。
	;;■これを変形すると　y^2 = ( 1 - x^2 / rx^2 ) * ry^2
	(sqrt (* (- 1 (/ (* px px) (* rx rx))) (* ry ry)))))

(defun ellipse-connect-point-LR-impl (cx cy rx ry y)
  (declare (ignore cx))
  ;;■まず、y から cy を引くことで、楕円の中心を原点として処理を簡略化する。
  (let ((py (- y cy)))
	;;■楕円は x^2 / rx^2 + y^2 / ry^2 = 1 とすることができる。
	;;■これを変形すると　x^2 = ( 1 - y^2 / ry^2 ) * rx^2
	(sqrt (* (- 1 (/ (* py py) (* ry ry))) (* rx rx)))))

(defun ellipse-connect-point-T (cx cy rx ry idx)
  (let* ((x (+ cx (* idx (/ rx 2))))
		 (y (ellipse-connect-point-TB-impl cx cy rx ry x)))
	(make-point x (- cy y))))

(defun ellipse-connect-point-B (cx cy rx ry idx)
  (let* ((x (+ cx (* idx (/ rx 2))))
		 (y (ellipse-connect-point-TB-impl cx cy rx ry x)))
	(make-point x (+ cy y))))

(defun ellipse-connect-point-L (cx cy rx ry idx)
  (let* ((y (+ cy (* idx (/ ry 2))))
		 (x (ellipse-connect-point-LR-impl cx cy rx ry y)))
	(make-point (- cx x) y)))

(defun ellipse-connect-point-R (cx cy rx ry idx)
  (let* ((y (+ cy (* idx (/ ry 2))))
		 (x (ellipse-connect-point-LR-impl cx cy rx ry y)))
	(make-point (+ cx x) y)))


(defun ellipse-connect-point (cx cy rx ry type arg)
  (let ((handler (ecase type
				   ((:center) #'ellipse-connect-point-C)
				   ((:top)    #'ellipse-connect-point-T)
				   ((:bottom) #'ellipse-connect-point-B)
				   ((:left)   #'ellipse-connect-point-L)
				   ((:right)  #'ellipse-connect-point-R))))
	(funcall handler cx cy rx ry arg)))


;-------------------------------------------------------------------------------
;
; shape ellipse
;
;-------------------------------------------------------------------------------
(defclass ellipse (shape)
  ((center-x	;:type     number
				:initform 0
				:initarg  :center-x
				:accessor shape-center)
   (center-y	;:type     number
				:initform 0
				:initarg  :center-y
				:accessor shape-middle)
   (radius-x	;:type     number
				:initform 0
				:initarg  :radius-x
				:accessor ellipse-radius-x)
   (radius-y	;:type     number
				:initform 0
				:initarg  :radius-y
				:accessor ellipse-radius-y)
   (fill		;:type     (or nil fill-info)
				:initform nil
				:initarg  :fill
				:accessor ellipse-fill)
   (stroke		;:type     (or nil stroke-info)
				:initform nil
				:initarg  :stroke
				:accessor ellipse-stroke)))

(defmethod initialize-instance :after ((ent ellipse) &rest initargs)
  (declare (ignore initargs))
  (with-slots (fill stroke) ent
	(setf fill (if (null fill)
				   *default-fill*
				   (make-fill fill)))
	(setf stroke (if (null stroke)
					 *default-stroke*
					 (make-stroke stroke))))
  ent)

(defmethod check ((shp ellipse) canvas dict)
  (declare (ignorable dict))
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (center-x center-y
					radius-x radius-y fill stroke) shp
	(check-member center-x  :nullable nil :types number)
	(check-member center-y  :nullable nil :types number)
	(check-member radius-x  :nullable nil :types number)
	(check-member radius-y  :nullable nil :types number)
	(check-object fill      canvas dict :nullable t :class   fill-info)
	(check-object stroke    canvas dict :nullable t :class stroke-info))
  (incf (shape-center shp) (canvas-left canvas))
  (incf (shape-middle shp) (canvas-top  canvas))
  nil)

(defmethod shape-width ((shp ellipse))
  (* 2 (ellipse-radius-x shp)))

(defmethod shape-height ((shp ellipse))
  (* 2 (ellipse-radius-y shp)))

(defmethod shape-top ((shp ellipse))
  (- (shape-middle shp) (ellipse-radius-y shp)))

(defmethod shape-bottom ((shp ellipse))
  (+ (shape-middle shp) (ellipse-radius-y shp)))

(defmethod shape-left ((shp ellipse))
  (- (shape-center shp) (ellipse-radius-x shp)))

(defmethod shape-right ((shp ellipse))
  (+ (shape-center shp) (ellipse-radius-x shp)))

;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((shp ellipse)) ...)

;;MEMO : use impelementation of shape...
;;(defmethod entity-composition-p ((shp ellipse)) ...)
  
(defmethod shape-connect-point ((shp ellipse) type arg)
  (ellipse-connect-point (shape-center     shp)
						 (shape-middle     shp)
						 (ellipse-radius-x shp)
						 (ellipse-radius-y shp) type arg))
  
(defmethod draw-entity ((shp ellipse) writer)
  (let ((cls  (shape-class shp))
		(id   (and (not (entity-composition-p shp))
				   (entity-id shp))))
	(pre-draw shp writer)
	(writer-write writer
				  "<ellipse "
				  (write-when id "id='" it "' ")
				  "cx='" (shape-center shp) "' "
				  "cy='" (shape-middle shp) "' "
				  "rx='" (ellipse-radius-x shp) "' "
				  "ry='" (ellipse-radius-y shp) "' "
				  (write-when cls "class='" it "' ")
				  (unless cls
					(let ((fill (ellipse-fill   shp)))
					  (when fill
						(to-property-strings fill))))
				  (unless cls
					(let ((strk (ellipse-stroke shp)))
					  (when strk
						(to-property-strings strk))))
				  "/>")
	(post-draw shp writer))
  nil)


#|
#|EXPORT|#				:ellipse
 |#
(defmacro ellipse (x y rx ry
				   &key class fill stroke link layer id contents)
  (let ((code `(register-entity (make-instance 'diagram:ellipse
											   :center-x ,x :center-y ,y 
											   :radius-x ,rx :radius-y ,ry :class ,class
											   :fill ,fill :stroke ,stroke
											   :link ,link :layer ,layer :id ,id))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (diagram:shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))

