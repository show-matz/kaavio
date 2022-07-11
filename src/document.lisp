#|
#|ASD|#				(:file "document"                  :depends-on ("cl-diagram"
#|ASD|#																"path"
#|ASD|#																"filter"
#|ASD|#																"text-shape"))
#|EXPORT|#				;document.lisp
 |#

(in-package :cl-diagram)

;;------------------------------------------------------------------------------
;;
;; class document
;;
;;------------------------------------------------------------------------------
(defclass document (text-shape)
  ((depth	:initform nil :initarg :depth)    ; number
   (filter	:initform nil :initarg :filter))) ; (or nil keyword)
  
(defmethod initialize-instance :after ((doc document) &rest initargs)
  (declare (ignore initargs))
  (with-slots (filter) doc
	(setf filter (or filter *default-shape-filter*)))
  doc)
   
(defmethod check ((doc document) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (depth filter height) doc
	(setf depth (or depth (/ height 3)))
	(check-member depth  :nullable nil :types number)
	(check-member filter :nullable   t :types keyword))
  nil)

;; override of group::draw-group
(defmethod draw-group ((doc document) writer)
  (let* ((canvas (group-get-canvas doc))
		 (w      (canvas-width  canvas))
		 (h      (canvas-height canvas)))
	(macrolet ((register-entity (entity)
				 `(check-and-draw-local-entity ,entity canvas writer)))
	  (with-slots (depth fill stroke filter) doc
		;; draw 
		(path `((:move-to ( 0  0))
				(:line-to ( 0 ,h))
				(:arc-to  ,(/ w 3) ,depth 0 0 0 (,(/ w 2) ,h))
				(:arc-to  ,(/ w 3) ,depth 1 0 1 (,w ,h))
				(:line-to (,w 0)) :close-path) :fill fill :stroke stroke :filter filter))))
  ;; draw text
  (call-next-method))

;; no override.
;(defmethod text-shape-calc-size ((doc document))
;  (call-next-method))

;; no override.
;(defmethod text-shape-paragraph-area ((doc document))
;  (call-next-method))

;;------------------------------------------------------------------------------
;;
;; macro document
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:document
 |#
(defmacro document (center width height text
						 &key depth font align valign fill stroke link rotate layer id filter contents)
  (let ((code `(register-entity (make-instance 'document
											   :center ,center :depth ,depth
											   :width ,width :height ,height
											   :text ,text :font ,font
											   :align ,align :valign ,valign
											   :fill ,fill :stroke ,stroke :link ,link 
											   :rotate ,rotate :layer ,layer
											   :filter ,filter :id ,id))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (diagram:shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))
