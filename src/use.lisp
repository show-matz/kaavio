#|
#|ASD|#				(:file "use"                       :depends-on ("cl-diagram"
#|ASD|#																"constants"
#|ASD|#																"canvas"
#|ASD|#																"point"
#|ASD|#																"shape"
#|ASD|#																"writer"))
#|EXPORT|#				;use.lisp
 |#

(in-package :cl-diagram)


;;-------------------------------------------------------------------------------
;;
;; class use
;;
;;-------------------------------------------------------------------------------
(defclass use (shape)
  ((ref			:initform nil :initarg :ref)		; keyword / shape
   (center		:initform nil :initarg :center)))	; point


;(defmethod initialize-instance :after ((ent use) &rest initargs)
;  (declare (ignore initargs))
;  ent)

(defmethod check ((ent use) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (ref center) ent
	(let ((obj (dict-get-entity dict ref)))
	  (if (and obj (typep obj 'diagram:defs))
		  (setf ref obj)
		  (throw-exception "ID '~A' is not found in dictionary or not defs object." ref)))
	(setf center (canvas-fix-point canvas center)))
  nil)

(defmethod shape-width ((obj use))
  (slot-value (slot-value obj 'ref) 'width))

(defmethod shape-height ((obj use))
  (slot-value (slot-value obj 'ref) 'height))

(defmethod shape-center ((obj use))
  (slot-value obj 'center))

;;MEMO : use impelementation of shape...
;;(defmethod shape-connect-point ((obj use) type1 type2 arg) ...)
  
;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((obj use)) ...)

(defmethod draw-entity ((obj use) writer)
  (with-slots (ref center) obj
	(pre-draw obj writer)
	(with-slots (width height) ref
	  (writer-write writer
					"<use xlink:href='#" (slot-value ref 'id) "' "
					"x='" (- (point-x center) (/ width  2)) "' "
					"y='" (- (point-y center) (/ height 2)) "' />"))
	(post-draw obj writer))
  nil)

;;------------------------------------------------------------------------------
;;
;; macro use
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:use
 |#
(defmacro use (ref center &key link layer id contents)
  (let ((code `(register-entity (make-instance 'diagram:use
											   :ref    ,ref  :center ,center
											   :link   ,link :layer  ,layer  :id ,id))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (diagram:shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))

