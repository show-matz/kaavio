#|
#|ASD|#				(:file "ellipse"                   :depends-on ("kaavio"
#|ASD|#																"constants"
#|ASD|#																"canvas"
#|ASD|#																"point"
#|ASD|#																"shape"
#|ASD|#																"stroke-info"
#|ASD|#																"link-info"
#|ASD|#																"filter"
#|ASD|#																"writer"))
#|EXPORT|#				;ellipse.lisp
 |#


(in-package :kaavio)

;;------------------------------------------------------------------------------
;;
;; utility functions
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:ellipse-connect-point
 |#
(defun ellipse-connect-point-C (cx cy rx ry pt)
  (with-point (pt-x pt-y) pt
	(if (= cx pt-x)
		;;■縦方向の直線になる場合は特別扱いする
		(if (< pt-y cy)
			(make-point cx (- cy ry) :absolute)
			(make-point cx (+ cy ry) :absolute))
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
			(make-point (+ x cx) (+ (* m x) cy) :absolute))))))

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
	(make-point x (- cy y) :absolute)))

(defun ellipse-connect-point-B (cx cy rx ry idx)
  (let* ((x (+ cx (* idx (/ rx 2))))
		 (y (ellipse-connect-point-TB-impl cx cy rx ry x)))
	(make-point x (+ cy y) :absolute)))

(defun ellipse-connect-point-L (cx cy rx ry idx)
  (let* ((y (+ cy (* idx (/ ry 2))))
		 (x (ellipse-connect-point-LR-impl cx cy rx ry y)))
	(make-point (- cx x) y :absolute)))

(defun ellipse-connect-point-R (cx cy rx ry idx)
  (let* ((y (+ cy (* idx (/ ry 2))))
		 (x (ellipse-connect-point-LR-impl cx cy rx ry y)))
	(make-point (+ cx x) y :absolute)))


(defun ellipse-connect-point (center rx ry type1 type2 arg)
  (declare (ignore type1))
  (let ((cx (point-x center))
		(cy (point-y center))
		(handler (ecase type2
				   ((:center) #'ellipse-connect-point-C)
				   ((:top)    #'ellipse-connect-point-T)
				   ((:bottom) #'ellipse-connect-point-B)
				   ((:left)   #'ellipse-connect-point-L)
				   ((:right)  #'ellipse-connect-point-R))))
	(funcall handler cx cy rx ry arg)))


;;------------------------------------------------------------------------------
;;
;; class ellipse
;;
;;------------------------------------------------------------------------------
(defclass ellipse (shape)
  ((center		:initform   0 :initarg :center)		; point
   (radius-x	:initform   0 :initarg :radius-x)	; number
   (radius-y	:initform   0 :initarg :radius-y)	; number
   (fill		:initform nil :initarg :fill)		; (or nil fill-info)
   (stroke		:initform nil :initarg :stroke)		; (or nil stroke-info)
   (filter		:initform nil :initarg :filter)))	; (or nil keyword)

(defmethod initialize-instance :after ((ent ellipse) &rest initargs)
  (declare (ignore initargs))
  (with-slots (fill stroke filter layer) ent
	(setf fill   (make-fill   (or fill   *default-fill*   :none)))
	(setf stroke (make-stroke (or stroke *default-stroke* :none)))
	(setf filter (if (eq filter :none)
					 nil
					 (or filter *default-shape-filter*)))
	(setf layer  (if (eq layer :none)
					 nil
					 (or layer *default-layer*))))
  ent)

(defmethod check ((shp ellipse) canvas dict)
  (declare (ignorable dict))
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (center radius-x radius-y fill stroke filter) shp
	(check-member radius-x  :nullable nil :types number)
	(check-member radius-y  :nullable nil :types number)
	(check-object fill      canvas dict :nullable nil :class   fill-info)
	(check-object stroke    canvas dict :nullable nil :class stroke-info)
	(check-member filter    :nullable   t :types keyword)
	(setf center (canvas-fix-point canvas center)))
  nil)

(defmethod shape-width ((shp ellipse))
  (* 2 (slot-value shp 'radius-x)))

(defmethod shape-height ((shp ellipse))
  (* 2 (slot-value shp 'radius-y)))

(defmethod shape-center ((shp ellipse))
  (slot-value shp 'center))


(defmethod shape-connect-point ((shp ellipse) type1 type2 arg)
  (ellipse-connect-point (shape-center   shp)
						 (slot-value shp 'radius-x)
						 (slot-value shp 'radius-y) type1 type2 arg))
  
;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((shp ellipse)) ...)

;;MEMO : use impelementation of shape...
;;(defmethod entity-composition-p ((shp ellipse)) ...)
  
(defmethod draw-entity ((shp ellipse) writer)
  (with-slots (center radius-x radius-y fill stroke filter) shp
	(let ((id (and (not (entity-composition-p shp))
				   (slot-value shp 'id))))
	  (pre-draw shp writer)
	  (writer-write writer
					"<ellipse "
					(write-when (keywordp id) "id='" id "' ")
					"cx='" (point-x center) "' "
					"cy='" (point-y center) "' "
					"rx='" radius-x "' "
					"ry='" radius-y "' "
					(to-property-strings fill)
					(to-property-strings stroke)
					(write-when filter "filter='url(#" it ")' ")
					"/>")
	  (post-draw shp writer)))
  nil)


;;------------------------------------------------------------------------------
;;
;; macro ellipse
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:ellipse
 |#
(defmacro ellipse (center rx ry
				   &key fill stroke rotate link layer id filter contents)
  (let ((code `(register-entity (make-instance 'kaavio:ellipse
											   :center ,center :rotate ,rotate
											   :radius-x ,rx :radius-y ,ry
											   :fill ,fill :stroke ,stroke
											   :filter ,filter :link ,link
											   :layer ,layer :id ,id))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (kaavio:shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))

