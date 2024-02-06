#|
#|ASD|#				(:file "memo"                      :depends-on ("kaavio"
#|ASD|#																"constants"
#|ASD|#																"arc"
#|ASD|#																"polygon"
#|ASD|#																"filter"
#|ASD|#																"text-shape"))
#|EXPORT|#				;memo.lisp
 |#

(in-package :kaavio)

#|
#|EXPORT|#				:*default-memo-crease*
#|EXPORT|#				:*default-memo-align*
#|EXPORT|#				:*default-memo-valign*
#|EXPORT|#				:*default-memo-margin*
#|EXPORT|#				:*default-memo-font*
#|EXPORT|#				:*default-memo-fill*
#|EXPORT|#				:*default-memo-fill2*
#|EXPORT|#				:*default-memo-stroke*
#|EXPORT|#				:*default-memo-filter*
#|EXPORT|#				:*default-memo-layer*
 |#
(defparameter *default-memo-crease*   20)
(defparameter *default-memo-align*    :center)
(defparameter *default-memo-valign*   :center)
(defparameter *default-memo-margin*   10)
(defparameter *default-memo-font*     nil)
(defparameter *default-memo-fill*     nil)
(defparameter *default-memo-fill2*    nil)
(defparameter *default-memo-stroke*   nil)
(defparameter *default-memo-filter*   nil)
(defparameter *default-memo-layer*    nil)


(defun memo-get-points1 (w h c)
  `((0         0)
	(0        ,h)
	(,(- w c) ,h)
	(,(- w (/ c 1.5)) ,(- h (/ c 1.5)))
	(,w       ,(- h (/ c 2)))
	(,w        0)))

(defun memo-get-points2 (w h c)
  `((,(- w c)       ,h)
	(,w             ,(- h (/ c 2)))
	(,(- w (/ c 1.5)) ,(- h (/ c 1.5)))))

(defun memo-get-stroke-points1 (w h c)
  `((0        ,(/ h 2))
	(0         0)
	(,w        0)
	(,w       ,(- h (/ c 2)))
	(,(- w c) ,h)
	(0        ,h)
	(0        ,(/ h 2))))

(defun memo-get-stroke-points2 (w h c)
  `((,w       ,(- h (/ c 2)))
	(,(- w (/ c 1.5)) ,(- h (/ c 1.5)))
	(,(- w c) ,h)))

;;------------------------------------------------------------------------------
;;
;; class memo
;;
;;------------------------------------------------------------------------------
(defclass memo (text-shape)
  ((crease	:initform nil :initarg :crease)  ; number
   (fill2	:initform nil :initarg :fill2)    ; (or nil fill-info)
   (filter	:initform nil :initarg :filter))) ; (or nil keyword)
  
(defmethod initialize-instance :after ((obj memo) &rest initargs)
  (declare (ignore initargs))
  (with-slots (crease fill2 filter layer) obj
	(setf crease (or crease *default-memo-crease*))
	(setf fill2  (make-fill (or fill2 *default-fill* :none)))
	(setf filter  (if (eq filter :none)
					  nil
					  (or filter *default-memo-filter* *default-filter*)))
	(setf layer  (if (eq layer :none)
					 nil
					 (or layer *default-memo-layer* *default-layer*))))
  obj)
   
(defmethod check ((obj memo) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (crease fill fill2 filter) obj
	(check-member crease    :nullable nil :types number)
	(setf fill2 (or fill2 fill))
	(check-object fill2  canvas dict :nullable t :class fill-info)
	(check-member filter    :nullable   t :types keyword))
  nil)

;; override of group::draw-group
(defmethod draw-group ((obj memo) writer)
  (let* ((canvas (group-get-canvas obj))
		 (width  (canvas-width  canvas))
		 (height (canvas-height canvas)))
	(macrolet ((register-entity (entity)
				 `(check-and-draw-local-entity ,entity canvas writer)))
	  (with-slots (crease fill fill2 stroke filter) obj
		;; draw
		(writer-write writer "<g stroke='none' "
							 (write-when filter "filter='url(#" it ")' ") ">")
		(writer-incr-level writer)
		(let ((*mute-stroke* t))
		  (polygon (memo-get-points1 width height crease) :fill fill)
		  (polygon (memo-get-points2 width height crease) :fill fill2))
		(writer-write writer "<g fill='none' "
							 (to-property-strings stroke) ">")
		(writer-incr-level writer)
		(let ((*mute-stroke* t))
		  (line (memo-get-stroke-points1 width height crease))
		  (line (memo-get-stroke-points2 width height crease)))
		(writer-decr-level writer)
		(writer-write writer "</g>")
		(writer-decr-level writer)
		(writer-write writer "</g>"))))
  ;; draw text
  (call-next-method))

;; no override.
;(defmethod text-shape-calc-size ((obj memo))
;  (call-next-method))

;; no override.
;(defmethod text-shape-paragraph-area ((obj memo))
;  (call-next-method))

;;------------------------------------------------------------------------------
;;
;; macro memo
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:memo
 |#
(defmacro memo (position text &key pivot width height crease align valign margin
								 font fill fill2 stroke link rotate layer id filter contents)
  (let* ((g-fill (gensym "FILL"))
		 (code `(let ((,g-fill ,fill))
				  (register-entity (make-instance 'memo
												   :crease ,crease
												   :position ,position :pivot ,pivot
												   :width ,width :height ,height
												   :text ,text
												   :align  (or ,align  *default-memo-align*)
												   :valign (or ,valign *default-memo-valign*)
												   :margin (or ,margin *default-memo-margin*)
												   :font   (or ,font   *default-memo-font*)
												   :fill   (or ,g-fill *default-memo-fill*)
												   :fill2  (or ,fill2
															   ,g-fill *default-memo-fill2*
																	   *default-memo-fill*)
												   :stroke (or ,stroke *default-memo-stroke*)
												   :link ,link :rotate ,rotate
												   :filter ,filter :layer ,layer :id ,id)))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (kaavio:shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))


;;------------------------------------------------------------------------------
;;
;; macro with-memo-options
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:with-memo-options
 |#
(defmacro with-memo-options ((&key crease align valign margin
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
	(let ((lst (impl (list crease '*default-memo-crease*
						   align  '*default-memo-align*
						   valign '*default-memo-valign*
						   margin '*default-memo-margin*
						   font   '*default-memo-font*
						   fill   '*default-memo-fill*
						   fill2  '*default-memo-fill2*
						   stroke '*default-memo-stroke*
						   filter '*default-memo-filter*
						   layer  '*default-memo-layer*) nil)))
	  `(let ,lst
		 ,@body))))
