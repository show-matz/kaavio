#|
#|ASD|#				(:file "raw-svg"                   :depends-on ("cl-diagram"
#|ASD|#																"constants"
#|ASD|#																"entity"
#|ASD|#																"writer"))
#|EXPORT|#				;raw-svg.lisp
 |#

(in-package :cl-diagram)

;;------------------------------------------------------------------------------
;;
;; class raw-svg
;;
;;------------------------------------------------------------------------------
(defclass raw-svg (entity)
  ((data :initform nil :initarg :svgdata)))	; string

(defmethod initialize-instance :after ((obj raw-svg) &rest initargs)
  (declare (ignore initargs))
  (with-slots (layer) obj
	(setf layer  (if (eq layer :none)
					 nil
					 (or layer *default-layer*))))
  obj)

(defmethod check ((ent raw-svg) canvas dict)
  (declare (ignorable canvas dict))
  (with-slots (data) ent
	(check-member data :nullable nil :types string)
	(setf data (fix-name data)))
  ;; this method must call super class' one.
  (call-next-method))

(defmethod draw-entity ((ent raw-svg) writer)
  (dolist (line (string/split (slot-value ent 'data) #\newline))
	(writer-write writer line)))



;;------------------------------------------------------------------------------
;;
;; macro raw-svg
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:raw-svg
 |#
(defmacro raw-svg (svgdata &key (layer nil))
  `(register-entity (make-instance 'diagram:raw-svg
								   :svgdata ,svgdata :layer ,layer)))

