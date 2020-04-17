#|
#|ASD|#				(:file "entity"                    :depends-on ("cl-diagram"
#|ASD|#																"writer"))
#|EXPORT|#				;entity.lisp
 |#

(in-package :cl-diagram)


;;------------------------------------------------------------------------------
;;
;; entity
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:entity
 |#
(defclass entity ()
  ((id		;:type     keyword
			:initform nil
			:initarg  :id
			:accessor entity-id)
   (layer	;:type     keyword
			:initform nil
			:initarg  :layer
			:accessor entity-layer)))

(defun begin-id-group (ent writer)
  (let ((id (entity-id ent)))
	(when id
	  (writer-write writer "<g id='" id "'>")
	  (writer-incr-level writer))))

(defun end-id-group (ent writer)
  (when (entity-id ent)
	(writer-decr-level writer)
	(writer-write writer "</g>")))


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
  (writer-write writer "<!-- "
					   (write-when (entity-id ent) it " : ")
					   (string-downcase (symbol-name (type-of ent)))
					   " -->"))

(defmethod pre-draw ((ent entity) writer)
  (when (entity-composition-p ent)
	(begin-id-group ent writer)))

(defmethod post-draw ((ent entity) writer)
  (when (entity-composition-p ent)
	(end-id-group ent writer)))

(defmethod check ((ent entity) canvas dict)
  (declare (ignore canvas dict))
  (with-slots (id layer) ent
	(check-member id    :nullable t :types keyword)
	(check-member layer :nullable t :types keyword)))
  



#|
#|EXPORT|#				:check-and-draw-local-entity
 |#
(defun check-and-draw-local-entity (entity canvas writer)
  (check entity canvas nil)		;; local entity can NOT use dictionary.
  (draw-entity entity writer))

