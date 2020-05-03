#|
#|ASD|#				(:file "writer"                    :depends-on ("cl-diagram"))
#|EXPORT|#				;writer.lisp
 |#

(in-package :cl-diagram)


;;------------------------------------------------------------------------------
;;
;; writer interface
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:writer-write
#|EXPORT|#				:writer-incr-level
#|EXPORT|#				:writer-decr-level
#|EXPORT|#				:writer-close
 |#
(defgeneric writer-write (writer &rest params))
(defgeneric writer-incr-level (writer))
(defgeneric writer-decr-level (writer))
(defgeneric writer-close (writer))

;;------------------------------------------------------------------------------
;;
;; class buffer-writer
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:buffer-writer
 |#
(defclass buffer-writer ()
  ((level  :type     integer
		   :initform 0
		   :initarg  :level
		   :accessor writer-level)
   (stream ;:type    number
		   :initform (make-string-output-stream)
		   :initarg  :stream
		   :accessor buffer-writer-stream)))

(defmethod writer-write ((writer buffer-writer) &rest params)
  (let ((stream (slot-value writer 'stream)))
	(dotimes (x (slot-value writer 'level))
	  (declare (ignorable x))
	  (princ #\tab stream))
	(dolist (itm params)
	  (__write-imp stream itm))
	(terpri stream)))

(defmethod writer-incr-level ((writer buffer-writer))
  (incf (slot-value writer 'level)))

(defmethod writer-decr-level ((writer buffer-writer))
  (decf (slot-value writer 'level)))

(defmethod writer-close ((writer buffer-writer))
  (let ((stream (slot-value writer 'stream)))
	(setf (slot-value writer 'stream) nil)
	(get-output-stream-string stream)))
	

;-------------------------------------------------------------------------------
; 
; function create-svg-writer
; 
;-------------------------------------------------------------------------------
#|
#|EXPORT|#				:create-svg-writer
 |#
; returns buffer-writer
; encoding --- :jis :euc-jp :sjis :utf8 :ascii :guess or :default
(defun create-svg-writer (#|file-name charset &key (eol-style :lf)|#)
;	(let ((writer (if (eq charset :default)
;					  (ol:new native-svg-writer file-name)
;					  (ol:new custom-svg-writer file-name
;							  (jp:make-encoding charset :eol-style eol-style)))))
	(make-instance 'buffer-writer))


