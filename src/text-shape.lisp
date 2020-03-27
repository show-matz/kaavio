#|
#|ASD|#				(:file "text-shape"                :depends-on ("cl-diagram"
#|ASD|#																"canvas"
#|ASD|#																"group"
#|ASD|#																"paragraph"
#|ASD|#																"font-info"
#|ASD|#																"fill-info"
#|ASD|#																"stroke-info"
#|ASD|#																"writer"))
#|EXPORT|#				;text-shape.lisp
 |#

(in-package :cl-diagram)

(defparameter *text-shape-font*       nil)
(defparameter *text-shape-fill*    :white)
(defparameter *text-shape-stroke*  :black)
(defparameter *text-shape-align*  :center)
(defparameter *text-shape-valign* :center)
(defparameter *text-shape-margin*      10)

;;------------------------------------------------------------------------------
;;
;; text-shape
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:text-shape
#|EXPORT|#				:text-shape-calc-size
#|EXPORT|#				:text-shape-paragraph-area
 |#
(defclass text-shape (group)
  ((text	;:type     (or keyword string)
			:initform nil
			:initarg  :text
			:accessor text-shape-text)
   (align	;:type     keyword
			:initform nil
			:initarg  :align
			:accessor text-shape-align)
   (valign	;:type     keyword
			:initform nil
			:initarg  :valign
			:accessor text-shape-valign)
   (font	;:type     (or nil font-info)
			:initform nil
			:initarg  :font
			:accessor text-shape-font)
   (fill	;:type     (or nil fill-info)
			:initform nil
			:initarg  :fill
			:accessor text-shape-fill)
   (stroke	;:type     (or nil stroke-info)
			:initform nil
			:initarg  :stroke
			:accessor text-shape-stroke)
   (margin	;:type     number
			:initform nil
			:initarg  :margin
			:accessor text-shape-margin)))


; calc width & height of whole text-shape.
(defgeneric text-shape-calc-size (txtshp))	;; returns (values w h)

; generate canvas for paragraph inner text-shape.
(defgeneric text-shape-paragraph-area (txtshp))	;; returns canvas



(defmethod initialize-instance :after ((txtshp text-shape) &rest initargs)
  (declare (ignore initargs))
  (with-slots (align valign font fill stroke margin) txtshp
	(setf align   (or align  *text-shape-align*))
	(setf valign  (or valign *text-shape-valign*))
	(setf font    (make-font   (or font   *text-shape-font*   *default-font*  )))
	(setf fill    (make-fill   (or fill   *text-shape-fill*   *default-fill*  )))
	(setf stroke  (make-stroke (or stroke *text-shape-stroke* *default-stroke*)))
	(setf margin  (or margin *text-shape-margin*)))
  txtshp)

(defmethod check ((txtshp text-shape) canvas dict)
  (check-member   (text    (text-shape-text   txtshp)) :nullable nil :types string)
  (check-member   (align   (text-shape-align  txtshp)) :nullable nil :types keyword)
  (check-member   (valign  (text-shape-valign txtshp)) :nullable nil :types keyword)
  (check-keywords (align   (text-shape-align  txtshp)) :left :center :right)
  (check-keywords (valign  (text-shape-valign txtshp)) :top  :center :bottom)
  (check-object   (font    (text-shape-font   txtshp)) canvas dict :nullable t :class   font-info)
  (check-object   (fill    (text-shape-fill   txtshp)) canvas dict :nullable t :class   fill-info)
  (check-object   (stroke  (text-shape-stroke txtshp)) canvas dict :nullable t :class stroke-info)
  (check-member   (margin  (text-shape-margin txtshp)) :nullable nil :types number)
  ;; width, height のいずれか（または両方）が省略されている場合は計算で決定
  ;;MEMO : 明示的に w/h を指定された場合、テキストがはみ出す可能性があるがそれは仕方ない
  (with-slots (width height) txtshp
	(unless (and width height)
	  (multiple-value-bind (w h) (text-shape-calc-size txtshp)
		(setf width  (or width  w))
		(setf height (or height h)))))
  ;; this method must call super class' one.
  ;;MEMO : w/h が明示的に指定された場合にそれが数値であるかのチェックは下記の group::check で実施
  (call-next-method))


;; override of group::draw-group
(defmethod draw-group ((txtshp text-shape) writer)
  ;;(draw-group-frame txtshp writer)    ; MEMO : for debug...
  (let ((canvas (text-shape-paragraph-area txtshp)))
	(declare (special canvas))
	(with-canvas (top bottom left right) canvas
	  (let ((width  (- right  left))
			(height (- bottom top)))
		(macrolet ((register-entity (entity)
					 (let ((g-entity (gensym "ENTITY")))
					   `(let ((,g-entity ,entity))
						  (setf (entity-canvas ,g-entity) canvas)
						  (check ,g-entity canvas nil)
						  (diagram:draw-entity ,g-entity writer)))))
		  (with-slots (text align valign font margin) txtshp
			;; draw text
			(let ((x (ecase align
					   ((:left)   margin)
					   ((:center) (/ width 2))
					   ((:right)  (- width margin))))
				  (y (ecase valign
					   ((:top)     margin)
					   ((:center) (/ height 2))
					   ((:bottom) (- height margin)))))
			  (paragraph x y text :align align :valign valign :font font)))))))
  nil)

(defmethod text-shape-calc-size ((txtshp text-shape))
  (let ((margin (text-shape-margin txtshp)))
	(multiple-value-bind (w h)
		(font-calc-textarea (text-shape-font txtshp) (text-shape-text txtshp))
	  (values (+ (* margin 2) w)
			  (+ (* margin 2) h)))))

(defmethod text-shape-paragraph-area ((txtshp text-shape))
  (copy-canvas (shape-canvas txtshp)))


;;(defmacro text-shape (x y text &key width height align valign font fill stroke margin link layer id)
;;  `(register-entity (make-instance 'text-shape
;;								   :center-x ,x :center-y ,y
;;								   :width ,width :height ,height
;;								   :text ,text :font ,font
;;								   :align ,align :valign ,valign
;;								   :fill ,fill :stroke ,stroke :margin ,margin
;;								   :link ,link :layer ,layer :id ,id)))

