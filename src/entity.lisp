#|
#|ASD|#				(:file "entity"                    :depends-on ("kaavio"
#|ASD|#																"canvas"
#|ASD|#																"writer"))
#|EXPORT|#				;entity.lisp
 |#

(in-package :kaavio)


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

