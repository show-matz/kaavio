#|
#|ASD|#				(:file "textbox"                   :depends-on ("kaavio"
#|ASD|#																"constants"
#|ASD|#																"rectangle"
#|ASD|#																"filter"
#|ASD|#																"text-shape"))
#|EXPORT|#				;textbox.lisp
 |#

(in-package :kaavio)

#|
#|EXPORT|#				:*default-textbox-rx*
#|EXPORT|#				:*default-textbox-ry*
#|EXPORT|#				:*default-textbox-align*
#|EXPORT|#				:*default-textbox-valign*
#|EXPORT|#				:*default-textbox-margin*
#|EXPORT|#				:*default-textbox-font*
#|EXPORT|#				:*default-textbox-fill*
#|EXPORT|#				:*default-textbox-stroke*
#|EXPORT|#				:*default-textbox-filter*
#|EXPORT|#				:*default-textbox-layer*
 |#
(defparameter *default-textbox-rx*           nil)
(defparameter *default-textbox-ry*           nil)
(defparameter *default-textbox-align*        :center)
(defparameter *default-textbox-valign*       :center)
(defparameter *default-textbox-margin*       10)
(defparameter *default-textbox-font*         nil)
(defparameter *default-textbox-fill*         nil)
(defparameter *default-textbox-stroke*       nil)
(defparameter *default-textbox-filter*       nil)
(defparameter *default-textbox-layer*        nil)

;;------------------------------------------------------------------------------
;;
;; class textbox
;;
;;------------------------------------------------------------------------------
(defclass textbox (text-shape)
  ((no-frame	:initform nil :initarg :no-frame)	; number
   (rx			:initform nil :initarg       :rx)	; number
   (ry			:initform nil :initarg       :ry)	; number
   (filter		:initform nil :initarg   :filter))) ; (or nil keyword)
  
(defmethod initialize-instance :after ((box textbox) &rest initargs)
  (declare (ignore initargs))
  (with-slots (filter layer) box
	(setf filter (if (eq filter :none)
					 nil
					 (or filter *default-textbox-filter* *default-filter*)))
	(setf layer  (if (eq layer :none)
					 nil
					 (or layer *default-textbox-layer* *default-layer*))))
  box)
   
;; override of group::draw-group
(defmethod draw-group ((box textbox) writer)
  (let* ((canvas (group-get-canvas box))
		 (width  (canvas-width  canvas))
		 (height (canvas-height canvas)))
	(macrolet ((register-entity (entity)
				 `(check-and-draw-local-entity ,entity canvas writer)))
	  (with-slots (no-frame rx ry fill stroke filter) box
		(unless no-frame
		  ;; draw box
		  (rectangle (list (/ width 2) (/ height 2)) width height
					 :rx rx :ry ry
					 :fill fill :stroke stroke :filter filter)))))
  ;; draw text
  (call-next-method))

;; no override.
;(defmethod text-shape-calc-size ((box textbox))
;  (call-next-method))

;; no override.
;(defmethod text-shape-paragraph-area ((box textbox))
;  (call-next-method))

;;------------------------------------------------------------------------------
;;
;; macro textbox
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:textbox
 |#
(defmacro textbox (center text &key width height no-frame rx ry
									align valign margin font fill stroke
									link rotate layer id filter contents)
  (let ((code `(register-entity (make-instance 'textbox
											   :no-frame ,no-frame
											   :center ,center
											   :width ,width :height ,height
											   :text ,text
											   :rx     (or ,rx     *default-textbox-rx*)
											   :ry     (or ,ry     *default-textbox-ry*)
											   :align  (or ,align  *default-textbox-align*)
											   :valign (or ,valign *default-textbox-valign*)
											   :margin (or ,margin *default-textbox-margin*)
											   :font   (or ,font   *default-textbox-font*)
											   :fill   (or ,fill   *default-textbox-fill*)
											   :stroke (or ,stroke *default-textbox-stroke*)
											   :link ,link  :rotate ,rotate
											   :filter ,filter :layer ,layer :id ,id))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (kaavio:shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))


;;------------------------------------------------------------------------------
;;
;; macro with-textbox-options
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:with-textbox-options
 |#
(defmacro with-textbox-options ((&key rx ry align valign margin
									  font fill stroke filter layer) &rest body)
  (labels ((impl (params acc)
			 (if (null params)
				 acc
				 (let ((value  (car  params))
					   (symbol (cadr params)))
				   (impl (cddr params)
						 (if (null value)
							 acc
							 (push (list symbol value) acc)))))))
	(let ((lst (impl (list rx     '*default-textbox-rx*
						   ry     '*default-textbox-ry*
						   align  '*default-textbox-align*
						   valign '*default-textbox-valign*
						   margin '*default-textbox-margin*
						   font   '*default-textbox-font*
						   fill   '*default-textbox-fill*
						   stroke '*default-textbox-stroke*
						   filter '*default-textbox-filter*
						   layer  '*default-textbox-layer*) nil)))
	  `(let ,lst
		 ,@body))))
