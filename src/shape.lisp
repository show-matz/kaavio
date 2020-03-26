#|
#|ASD|#				(:file "shape"                     :depends-on ("cl-diagram"
#|ASD|#																"canvas"
#|ASD|#																"entity"
#|ASD|#																"link-info"))
#|EXPORT|#				;shape.lisp
 |#

(in-package :cl-diagram)

;;------------------------------------------------------------------------------
;;
;; class shape
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:shape
 |#
(defclass shape (entity)
  ((class	;:type     keyword
			:initform nil
			:initarg  :class
			:accessor shape-class)
   (link	;:type     (or nil link-info)
			:initform nil
			:initarg  :link
			:accessor shape-link)))

(defmethod initialize-instance :after ((shp shape) &rest initargs)
  (declare (ignore initargs))
  (with-slots (link) shp
	(setf link (make-link link)))
  shp)

(defmethod entity-composition-p ((shp shape))
  (not (null (shape-link shp))))

#|
#|EXPORT|#				:shape-width
#|EXPORT|#				:shape-height
#|EXPORT|#				:shape-center
#|EXPORT|#				:shape-middle
#|EXPORT|#				:shape-top
#|EXPORT|#				:shape-bottom
#|EXPORT|#				:shape-left
#|EXPORT|#				:shape-right
#|EXPORT|#				:shape-canvas
#|EXPORT|#				:get-cc-point
 |#
(defgeneric shape-width  (shp))
(defgeneric shape-height (shp))
(defgeneric shape-center (shp))
(defgeneric shape-middle (shp))
(defgeneric shape-top    (shp))
(defgeneric shape-bottom (shp))
(defgeneric shape-left   (shp))
(defgeneric shape-right  (shp))
(defgeneric shape-canvas (shp))
(defgeneric get-cc-point (shp x y))

(defmethod shape-top ((shp shape))
  (- (shape-middle shp) (/ (shape-height shp) 2)))

(defmethod shape-bottom ((shp shape))
  (+ (shape-middle shp) (/ (shape-height shp) 2)))

(defmethod shape-left ((shp shape))
  (- (shape-center shp) (/ (shape-width shp) 2)))

(defmethod shape-right ((shp shape))
  (+ (shape-center shp) (/ (shape-width shp) 2)))

(defmethod shape-canvas ((shp shape))
  (make-canvas (shape-top    shp)
			   (shape-bottom shp)
			   (shape-left   shp)
			   (shape-right  shp)))

(defmethod pre-draw ((shp shape) writer)
  (call-next-method)
  (when (entity-composition-p shp)
	(let ((lnk (shape-link shp)))
	  (when lnk
		(write-link-open lnk writer)))))

(defmethod post-draw ((shp shape) writer)
  (when (entity-composition-p shp)
	(let ((lnk (shape-link shp)))
	  (when lnk
		(write-link-close lnk writer))))
  (call-next-method))


(defmethod check ((shp shape) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (check-member (class (shape-class shp)) :nullable t :types (or keyword string))
  (check-object (link  (shape-link  shp)) canvas dict :nullable t :class link-info)
  nil)
  

