#|
#|ASD|#				(:file "css"                       :depends-on ("cl-diagram"
#|ASD|#																"entity"
#|ASD|#																"writer"))
#|EXPORT|#				;css.lisp
 |#

(in-package :cl-diagram)

(defclass css (entity)
  ((styles	;:type     string
			:initform ""
			:initarg  :styles
			:accessor css-styles)))

(defmethod check ((ent css) canvas dict)
  (declare (ignorable canvas dict))
  ;; this method must call super class' one.
  (call-next-method)
  (check-member (styles (css-styles ent)) :nullable nil :types string)
  nil)

(defmethod draw-entity ((ent css) writer)
  (writer-write writer "<style type='text/css'><![CDATA[")
  (writer-incr-level writer)
  (dolist (line (string/split (css-styles ent) #\newline))
	(writer-write writer line))
  (writer-decr-level writer)
  (writer-write writer "]]></style>"))


#|
#|EXPORT|#				:css
|#
(defmacro css (styles &key layer)
`(register-entity (make-instance 'diagram:css
								 :styles ,styles :layer ,layer)))

