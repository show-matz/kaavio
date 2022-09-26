#|
#|ASD|#				(:file "diamond"                   :depends-on ("kaavio"
#|ASD|#																"constants"
#|ASD|#																"canvas"
#|ASD|#																"point"
#|ASD|#																"shape"
#|ASD|#																"stroke-info"
#|ASD|#																"link-info"
#|ASD|#																"filter"
#|ASD|#																"writer"))
#|EXPORT|#				;diamond.lisp
 |#


(in-package :kaavio)

;;-------------------------------------------------------------------------------
;;
;; class diamond
;;
;;-------------------------------------------------------------------------------
(defclass diamond (shape)
  ((center		:initform nil :initarg :center)		; point
   (width		:initform   0 :initarg :width)		; number
   (height		:initform   0 :initarg :height)		; number
   (fill		:initform nil :initarg :fill)		; (or nil fill-info)
   (stroke		:initform nil :initarg :stroke)		; (or nil fill-info)
   (filter		:initform nil :initarg :filter)))	; (or nil keyword)


(defmethod initialize-instance :after ((rct diamond) &rest initargs)
  (declare (ignore initargs))
  (with-slots (fill stroke filter layer) rct
	(setf fill   (make-fill   (or fill   *default-fill*   :none)))
	(setf stroke (make-stroke (or stroke *default-stroke* :none)))
	(setf filter (if (eq filter :none)
					 nil
					 (or filter *default-shape-filter*)))
	(setf layer  (if (eq layer :none)
					 nil
					 (or layer *default-layer*))))
  rct)

(defmethod check ((rct diamond) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (center width height fill stroke filter) rct
	(check-member width     :nullable nil :types number)
	(check-member height    :nullable nil :types number)
	(check-object fill      canvas dict :nullable nil :class   fill-info)
	(check-object stroke    canvas dict :nullable nil :class stroke-info)
	(check-member filter    :nullable   t :types keyword)
	(setf center (canvas-fix-point canvas center)))
  nil)

(defmethod shape-width ((rct diamond))
  (slot-value rct 'width))

(defmethod shape-height ((rct diamond))
  (slot-value rct 'height))

(defmethod shape-center ((rct diamond))
  (slot-value rct 'center))

;;MEMO : use impelementation of shape...
;;(defmethod shape-connect-point ((shp diamond) type1 type2 arg) ...)
  
;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((shp diamond)) ...)

(defmethod draw-entity ((rct diamond) writer)
  (labels ((format-points (pts)
			 (with-output-to-string (stream)
			   (do ((idx 0 (incf idx)))
				   ((null pts) nil)
				 (unless (zerop idx)
				   (princ #\space stream))
				 (format stream "~A,~A"
						 (coerce (point-x (car pts)) 'single-float)
						 (coerce (point-y (car pts)) 'single-float))
				 (setf pts (cdr pts))))))
	(with-slots (center width height fill stroke filter) rct
	  (let ((id (and (not (entity-composition-p rct))
					 (slot-value rct 'id)))
			(topleft (shape-topleft rct))
			(points  (list (y+ center (- (/ height 2)))
						   (x+ center (- (/ width  2)))
						   (y+ center (+ (/ height 2)))
						   (x+ center (+ (/ width  2)))
						   (y+ center (- (/ height 2))))))
		(pre-draw rct writer)
		(writer-write writer
					  "<polygon "
					  (write-when id "id='" it "' ")
					  (to-property-strings fill)
					  (to-property-strings stroke)
					  "points='" (format-points points) "' "
					  (write-when filter "filter='url(#" it ")' ")
					  "/>")
		(post-draw rct writer))))
  nil)
  

;;-------------------------------------------------------------------------------
;;
;; macro diamond
;;
;;-------------------------------------------------------------------------------
#|
#|EXPORT|#				:diamond
 |#
(defmacro diamond (center width height
					 &key fill stroke rotate link layer id filter contents)
  (let ((code `(register-entity (make-instance 'kaavio:diamond
											   :center ,center
											   :width ,width :height ,height
											   :fill ,fill :stroke ,stroke
											   :rotate ,rotate :link ,link
											   :filter ,filter :layer ,layer :id ,id))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (kaavio:shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))

