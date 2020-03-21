#|
#|ASD|#				(:file "raw-svg"                   :depends-on ("cl-diagram"
#|ASD|#																"entity"
#|ASD|#																"writer"))
#|EXPORT|#				;raw-svg.lisp
 |#

(in-package :cl-diagram)

(defclass raw-svg (entity)
  ((data	;:type     string
			:initform ""
			:initarg  :svgdata
			:accessor raw-svg-data)))

(defmethod check ((ent raw-svg) canvas dict)
  (declare (ignorable canvas dict))
  ;; this method must call super class' one.
  (call-next-method)
  (check-member (svgdata (raw-svg-data ent)) :nullable nil :types string)
  nil)

(defmethod draw-entity ((ent raw-svg) writer)
  (dolist (line (string/split (raw-svg-data ent) #\newline))
	(writer-write writer line)))



#|
#|EXPORT|#				:raw-svg
 |#
(defmacro raw-svg (svgdata &key (layer nil))
  `(register-entity (make-instance 'diagram:raw-svg
								   :svgdata ,svgdata :layer ,layer)))

