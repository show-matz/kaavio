#|
#|ASD|#				(:file "stylesheet"                :depends-on ("cl-diagram"
#|ASD|#																"entity"
#|ASD|#																"stroke-info"
#|ASD|#																"fill-info"
#|ASD|#																"font-info"
#|ASD|#																"writer"))
#|EXPORT|#				;stylesheet.lisp
 |#

(in-package :cl-diagram)

;;------------------------------------------------------------------------------
;;
;; style-info
;;
;;------------------------------------------------------------------------------
(defclass style-info ()
  ;;ToDo : fill / stroke / font 以外のスタイルをどう扱うか．．．
  ((target	;:type     (or keyword string)
			:initform nil
			:initarg  :target
			:accessor style-target)
   (fill	;:type     (or nil fill-info)
			:initform nil
			:initarg  :fill
			:accessor style-fill)
   (stroke	;:type     (or nil link-info)
			:initform nil
			:initarg  :stroke
			:accessor style-stroke)
   (font	;:type     (or nil font-info)
			:initform nil
			:initarg  :font
			:accessor style-font)))

(defmethod initialize-instance :after ((obj style-info) &rest initargs)
  (declare (ignore initargs))
  (with-slots (fill stroke font) obj
	(setf fill   (and fill   (make-fill     fill)))
	(setf stroke (and stroke (make-stroke stroke)))
	(setf font   (and font   (make-font     font))))
  obj)

(defmethod check ((obj style-info) canvas dict)
  (with-slots (target fill stroke font) obj
	(check-member target :nullable nil :types (or keyword string))
	(check-object fill   canvas dict :nullable t :class   fill-info)
	(check-object stroke canvas dict :nullable t :class stroke-info)
	(check-object font   canvas dict :nullable t :class   font-info))
  nil)

(defmethod to-style-strings ((obj style-info))
  (let ((fill   (style-fill   obj))
		(stroke (style-stroke obj))
		(font   (style-font   obj)))
	(setf fill   (and fill   (to-style-strings   fill)))
	(setf stroke (and stroke (to-style-strings stroke)))
	(setf font   (and font   (to-style-strings   font)))
	(format-string (style-target obj) " { " fill stroke font " }")))

#|
#|EXPORT|#				:style
 |#
(defmacro style (target &key fill stroke font)
  `(make-instance 'style-info
				  :target ,target :fill ,fill :stroke ,stroke :font ,font))


;;------------------------------------------------------------------------------
;;
;; stylesheet
;;
;;------------------------------------------------------------------------------
(defclass stylesheet (entity)
  ((styles	;:type     list of style-info
			:initform nil
			:initarg  :styles
			:accessor stylesheet-styles)))

;;MEMO : currently do nothing...
;;(defmethod initialize-instance :after ((sht stylesheet) &rest initargs)
;;  (declare (ignore initargs))
;;  obj)
		   
(defmethod check ((sht stylesheet) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (dolist (entry (stylesheet-styles sht))
	(check entry canvas dict))
  nil)

(defmethod draw-entity ((sht stylesheet) writer)
  (let ((entries (stylesheet-styles sht)))
	(when entries
	  (writer-write writer "<style type='text/css'><![CDATA[")
	  (writer-incr-level writer)
	  (dolist (entry entries)
		(writer-write writer (to-style-strings entry)))
	  (writer-decr-level writer)
	  (writer-write writer "]]></style>"))))


#|
#|EXPORT|#				:stylesheet
|#
(defmacro stylesheet (&rest styles)
  `(register-entity (make-instance 'diagram:stylesheet :styles (list ,@styles))))

