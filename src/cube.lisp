#|
#|ASD|#				(:file "cube"                      :depends-on ("kaavio"
#|ASD|#																"constants"
#|ASD|#																"path"
#|ASD|#																"filter"
#|ASD|#																"text-shape"))
#|EXPORT|#				;cube.lisp
 |#

(in-package :kaavio)

#|
#|EXPORT|#				:*default-cube-depth*
#|EXPORT|#				:*default-cube-align*
#|EXPORT|#				:*default-cube-valign*
#|EXPORT|#				:*default-cube-margin*
#|EXPORT|#				:*default-cube-font*
#|EXPORT|#				:*default-cube-fill*
#|EXPORT|#				:*default-cube-fill2*
#|EXPORT|#				:*default-cube-stroke*
#|EXPORT|#				:*default-cube-filter*
#|EXPORT|#				:*default-cube-layer*
 |#
(defparameter *default-cube-depth*         nil)
(defparameter *default-cube-align*     :center)
(defparameter *default-cube-valign*    :center)
(defparameter *default-cube-margin*         10)
(defparameter *default-cube-font*          nil)
(defparameter *default-cube-fill*          nil)
(defparameter *default-cube-fill2*         nil)
(defparameter *default-cube-stroke*        nil)
(defparameter *default-cube-filter*        nil)
(defparameter *default-cube-layer*         nil)

;;------------------------------------------------------------------------------
;;
;; class cube
;;
;;------------------------------------------------------------------------------
(defclass cube (text-shape)
  ((depth	:initform nil :initarg :depth)    ; number
   (fill2	:initform nil :initarg :fill2)    ; (or nil fill-info)
   (filter	:initform nil :initarg :filter))) ; (or nil keyword)

(defmethod initialize-instance :after ((cb cube) &rest initargs)
  (declare (ignore initargs))
  (with-slots (fill2 filter layer) cb
	(setf fill2  (make-fill (or fill2 *default-fill* :none)))
	(setf filter (if (eq filter :none)
					 nil
					 (or filter *default-cube-filter* *default-filter*)))
	(setf layer  (if (eq layer :none)
					 nil
					 (or layer *default-cube-layer* *default-layer*))))
  cb)

(defmethod check ((cb cube) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (depth fill fill2 filter height) cb
	(setf depth (or depth (/ height 5)))
	(check-member depth  :nullable nil :types number)
	(setf fill2 (or fill2 fill))
	(check-object fill2  canvas dict :nullable t :class fill-info)
	(check-member filter :nullable   t :types keyword))
  nil)

(defmethod shape-get-subcanvas ((cb cube))
  (with-slots (depth) cb
	(let ((half (/ depth 2)))
	  (make-canvas (point/y+ (attribute-topleft cb) half)
				   (- (attribute-width  cb) half)
				   (- (attribute-height cb) half)))))

;; override of group::draw-group
(defmethod draw-group ((cb cube) writer)
  (let ((canvas (group-get-canvas cb)))
	(with-slots (depth contents-p fill fill2 stroke filter) cb
	  (let* ((width     (canvas-width  canvas))
			 (height    (canvas-height canvas))
			 (x         (/ width  2))
			 (y         (/ height 2))
			 (half      (/ depth  2)))
		(macrolet ((register-entity (entity)
					 `(check-and-draw-local-entity ,entity canvas writer)))
		  (writer-write writer "<g stroke='none' "
							   (write-when filter "filter='url(#" it ")' ") ">")
		  (writer-incr-level writer)
		  (let ((*mute-stroke* t))
			(polygon `((0 ,half)
					   (,depth ,(- half))
					   (,(+ width half) ,(- half))
					   (,(+ width half) ,(- height depth))
					   (,(- width half) ,height)
					   (,(- width half) ,half)) :fill fill2)
			(rectangle (make-point (- x (/ half 2))
								   (+ y (/ half 2)))
					   (- width half) (- height half) :fill fill))
		  (path `((:move-to (0 ,half))
				  (:line-to (,(- width half) ,half)
							(,(- width half) ,height)
							(0               ,height)
							(0               ,half)
							(,depth          ,(- half))
							(,(+ width half) ,(- half))
							(,(+ width half) ,(- height depth))
							(,(- width half) ,height))
				  (:move-to (,(- width half) ,half))
				  (:line-to (,(+ width half) ,(- half))))
				:stroke stroke :fill :none)
		  (writer-decr-level writer)
		  (writer-write writer "</g>")))))
  ;; draw text
  (call-next-method))

;; no override.
;(defmethod text-shape-calc-size ((cb cube))
;  (call-next-method))

(defmethod text-shape-paragraph-area ((cb cube))
  (shape-get-subcanvas cb))

;;------------------------------------------------------------------------------
;;
;; macro cube
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:cube
 |#
(defmacro cube (center width height text
					&key depth align valign margin
						 font fill fill2 stroke link rotate layer id filter contents)
  (let* ((g-fill (gensym "FILL"))
		 (code `(let ((,g-fill ,fill))
				  (register-entity (make-instance 'cube
												   :center ,center :text ,text
												   :width ,width :height ,height
	 											   :depth  (or ,depth  *default-cube-depth*)
	 											   :align  (or ,align  *default-cube-align*)
	 											   :valign (or ,valign *default-cube-valign*)
	 											   :margin (or ,margin *default-cube-margin*)
												   :font   (or ,font   *default-cube-font*)
												   :fill   (or ,g-fill *default-cube-fill*)
												   :fill2  (or ,fill2
															   ,g-fill *default-cube-fill2*
																	   *default-cube-fill*)
												   :stroke (or ,stroke *default-cube-stroke*)
												   :link ,link  :rotate ,rotate
												   :filter ,filter :layer ,layer :id ,id)))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))

;;------------------------------------------------------------------------------
;;
;; macro with-cube-options
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:with-cube-options
 |#
(defmacro with-cube-options ((&key depth align valign margin
								   font fill fill2 stroke filter layer) &rest body)
  (labels ((impl (params acc)
			 (if (null params)
				 acc
				 (let ((value  (car  params))
					   (symbol (cadr params)))
				   (impl (cddr params)
						 (if (null value)
							 acc
							 (push (list symbol value) acc)))))))
	(let ((lst (impl (list depth  '*default-cube-depth*
						   align  '*default-cube-align*
						   valign '*default-cube-valign*
						   margin '*default-cube-margin*
						   font   '*default-cube-font*
						   fill   '*default-cube-fill*
						   fill2  '*default-cube-fill2*
						   stroke '*default-cube-stroke*
						   filter '*default-cube-filter*
						   layer  '*default-cube-layer*) nil)))
	  `(let ,lst
		 ,@body))))
