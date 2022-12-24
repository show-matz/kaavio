#|
#|ASD|#				(:file "entity"                    :depends-on ("kaavio"
#|ASD|#																"canvas"
#|ASD|#																"point"
#|ASD|#																"writer"))
#|EXPORT|#				;entity.lisp
 |#

(in-package :kaavio)


#|
#|EXPORT|#				:attribute-id
#|EXPORT|#				:attribute-width
#|EXPORT|#				:attribute-height
#|EXPORT|#				:attribute-topleft
#|EXPORT|#				:attribute-top
#|EXPORT|#				:attribute-topright
#|EXPORT|#				:attribute-left
#|EXPORT|#				:attribute-center
#|EXPORT|#				:attribute-right
#|EXPORT|#				:attribute-bottomleft
#|EXPORT|#				:attribute-bottom
#|EXPORT|#				:attribute-bottomright
#|EXPORT|#				:attribute-end1
#|EXPORT|#				:attribute-end2
 |#
(defgeneric attribute-id          (shp))	;; returns symbol (keyword or gensym).
(defgeneric attribute-width       (shp))	;; returns number.
(defgeneric attribute-height      (shp))	;; returns number.
(defgeneric attribute-topleft     (shp))	;; returns point object.
(defgeneric attribute-top         (shp))	;; returns point object.
(defgeneric attribute-topright    (shp))	;; returns point object.
(defgeneric attribute-left        (shp))	;; returns point object.
(defgeneric attribute-center      (shp))	;; returns point object.
(defgeneric attribute-right       (shp))	;; returns point object.
(defgeneric attribute-bottomleft  (shp))	;; returns point object.
(defgeneric attribute-bottom      (shp))	;; returns point object.
(defgeneric attribute-bottomright (shp))	;; returns point object.
(defgeneric attribute-end1        (shp))	;; returns point object.
(defgeneric attribute-end2        (shp))	;; returns point object.

(defun attribute-topleft.X     (shp) (point-x (attribute-topleft     shp)))
(defun attribute-topleft.Y     (shp) (point-y (attribute-topleft     shp)))
(defun attribute-top.X         (shp) (point-x (attribute-top         shp)))
(defun attribute-top.Y         (shp) (point-y (attribute-top         shp)))
(defun attribute-topright.X    (shp) (point-x (attribute-topright    shp)))
(defun attribute-topright.Y    (shp) (point-y (attribute-topright    shp)))
(defun attribute-left.X        (shp) (point-x (attribute-left        shp)))
(defun attribute-left.Y        (shp) (point-y (attribute-left        shp)))
(defun attribute-center.X      (shp) (point-x (attribute-center      shp)))
(defun attribute-center.Y      (shp) (point-y (attribute-center      shp)))
(defun attribute-right.X       (shp) (point-x (attribute-right       shp)))
(defun attribute-right.Y       (shp) (point-y (attribute-right       shp)))
(defun attribute-bottomleft.X  (shp) (point-x (attribute-bottomleft  shp)))
(defun attribute-bottomleft.Y  (shp) (point-y (attribute-bottomleft  shp)))
(defun attribute-bottom.X      (shp) (point-x (attribute-bottom      shp)))
(defun attribute-bottom.Y      (shp) (point-y (attribute-bottom      shp)))
(defun attribute-bottomright.X (shp) (point-x (attribute-bottomright shp)))
(defun attribute-bottomright.Y (shp) (point-y (attribute-bottomright shp)))
(defun attribute-end1.X        (shp) (point-x (attribute-end1        shp)))
(defun attribute-end1.Y        (shp) (point-y (attribute-end1        shp)))
(defun attribute-end2.X        (shp) (point-x (attribute-end2        shp)))
(defun attribute-end2.Y        (shp) (point-y (attribute-end2        shp)))

(defun attribute-W   (shp) (attribute-width       shp))
(defun attribute-H   (shp) (attribute-height      shp))
(defun attribute-TL  (shp) (attribute-topleft     shp))
(defun attribute-TC  (shp) (attribute-top         shp))
(defun attribute-TR  (shp) (attribute-topright    shp))
(defun attribute-CL  (shp) (attribute-left        shp))
(defun attribute-CC  (shp) (attribute-center      shp))
(defun attribute-CR  (shp) (attribute-right       shp))
(defun attribute-BL  (shp) (attribute-bottomleft  shp))
(defun attribute-BC  (shp) (attribute-bottom      shp))
(defun attribute-BR  (shp) (attribute-bottomright shp))

(defun attribute-TL.X (shp) (point-x (attribute-topleft     shp)))
(defun attribute-TL.Y (shp) (point-y (attribute-topleft     shp)))
(defun attribute-TC.X (shp) (point-x (attribute-top         shp)))
(defun attribute-TC.Y (shp) (point-y (attribute-top         shp)))
(defun attribute-TR.X (shp) (point-x (attribute-topright    shp)))
(defun attribute-TR.Y (shp) (point-y (attribute-topright    shp)))
(defun attribute-CL.X (shp) (point-x (attribute-left        shp)))
(defun attribute-CL.Y (shp) (point-y (attribute-left        shp)))
(defun attribute-CC.X (shp) (point-x (attribute-center      shp)))
(defun attribute-CC.Y (shp) (point-y (attribute-center      shp)))
(defun attribute-CR.X (shp) (point-x (attribute-right       shp)))
(defun attribute-CR.Y (shp) (point-y (attribute-right       shp)))
(defun attribute-BL.X (shp) (point-x (attribute-bottomleft  shp)))
(defun attribute-BL.Y (shp) (point-y (attribute-bottomleft  shp)))
(defun attribute-BC.X (shp) (point-x (attribute-bottom      shp)))
(defun attribute-BC.Y (shp) (point-y (attribute-bottom      shp)))
(defun attribute-BR.X (shp) (point-x (attribute-bottomright shp)))
(defun attribute-BR.Y (shp) (point-y (attribute-bottomright shp)))


;;------------------------------------------------------------------------------
;;
;; abstract class entity
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:entity
 |#
(defclass entity ()
  ((id		:initform nil :initarg :id)			; keyword or gensym
   (layer	:initform nil :initarg :layer)))	; keyword


#|
#|EXPORT|#				:write-header
#|EXPORT|#				:draw-entity
#|EXPORT|#				:pre-draw
#|EXPORT|#				:post-draw
#|EXPORT|#				:entity-composition-p
 |#
(defgeneric entity-composition-p (ent))
(defgeneric write-header (entity writer))
(defgeneric pre-draw (entity writer))
(defgeneric post-draw (entity writer))
(defgeneric draw-entity (entity writer))

(defmethod entity-composition-p ((ent entity))
  (declare (ignore ent))
  nil)

(defmethod write-header ((ent entity) writer)
  (with-slots (id) ent
	(writer-write writer "<!-- "
						 (write-when (keywordp id) id " : ")
						 (string-downcase (symbol-name (type-of ent)))
						 " -->")))

(defmethod pre-draw ((ent entity) writer)
  (when (entity-composition-p ent)
	(let ((id (slot-value ent 'id)))
	  (when (keywordp id)
		(writer-write writer "<g id='" id "'>")
		(writer-incr-level writer)))))

(defmethod post-draw ((ent entity) writer)
  (when (entity-composition-p ent)
	(when (keywordp (slot-value ent 'id))
	  (writer-decr-level writer)
	  (writer-write writer "</g>"))))

(defmethod check ((ent entity) canvas dict)
  (declare (ignore canvas dict))
  (with-slots (id layer) ent
	(check-member id    :nullable t :types keyword)
	(unless id
	  (setf id (gensym "ENTITY")))
	(check-member layer :nullable t :types keyword)))
  



#|
#|EXPORT|#				:check-and-draw-local-entity
 |#
(defun check-and-draw-local-entity (entity canvas writer)
  (check entity canvas nil)		;; local entity can NOT use dictionary.
  (draw-entity entity writer))

