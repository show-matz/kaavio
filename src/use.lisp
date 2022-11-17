#|
#|ASD|#				(:file "use"                       :depends-on ("kaavio"
#|ASD|#																"constants"
#|ASD|#																"defgroup"
#|ASD|#																"canvas"
#|ASD|#																"point"
#|ASD|#																"shape"
#|ASD|#																"writer"))
#|EXPORT|#				;use.lisp
 |#

(in-package :kaavio)


;;-------------------------------------------------------------------------------
;;
;; class use
;;
;;-------------------------------------------------------------------------------
(defclass use (shape)
  ((ref			:initform nil :initarg :ref)		; keyword / shape
   (center		:initform nil :initarg :center)		; point
   (debug		:initform nil :initarg :debug)))	; (or nil t keyword)


(defmethod initialize-instance :after ((ent use) &rest initargs)
  (declare (ignore initargs))
  (with-slots (debug) ent
	(when debug
	  (setf debug (if (keywordp debug) debug :red))))
  ent)

(defmethod check ((ent use) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (ref center layer debug) ent
	(let ((obj (dict-get-entity dict ref)))
	  (if (and obj (typep obj 'kaavio:group-definition))
		  (setf ref obj)
		  (throw-exception "ID '~A' is not found in dictionary or not defgroup object." ref)))
	(setf center (canvas-fix-point canvas center))
	(setf layer  (if (eq layer :none)
					 nil
					 (or layer *default-layer*)))
	(check-member debug  :nullable t :types keyword))
  nil)

(defmethod attribute-width ((obj use))
  (slot-value (slot-value obj 'ref) 'width))

(defmethod attribute-height ((obj use))
  (slot-value (slot-value obj 'ref) 'height))

(defmethod attribute-center ((obj use))
  (slot-value obj 'center))

;;MEMO : use impelementation of shape...
;;(defmethod shape-connect-point ((obj use) type1 type2 arg) ...)
  
;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((obj use)) ...)

(defmethod draw-entity ((obj use) writer)
  (with-slots (ref center debug) obj
	(pre-draw obj writer)
	(with-slots (width height) ref
	  (writer-write writer
					"<use xlink:href='#" (slot-value ref 'id) "' "
					"x='" (- (point-x center) (/ width  2)) "' "
					"y='" (- (point-y center) (/ height 2)) "' />")
	  (when debug
		(writer-write writer
					  "<rect "
					  "x='" (- (point-x center) (/ width 2)) "' "
					  "y='" (- (point-y center) (/ height 2)) "' "
					  "width='" width "' "
					  "height='" height "' "
					  "fill='none' "
					  (to-property-strings (make-stroke :color debug :dasharray '(1 4)))
					  "/>")))
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
(defmacro use (ref center &key link rotate layer id contents debug)
  (let ((code `(register-entity (make-instance 'kaavio:use
											   :ref   ,ref   :center ,center
											   :link  ,link  :rotate ,rotate
											   :layer ,layer :id ,id :debug ,debug))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (kaavio:shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))

