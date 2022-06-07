#|
#|ASD|#				(:file "shadow-filter"             :depends-on ("cl-diagram"
#|ASD|#																"filter"
#|ASD|#																"writer"))
#|EXPORT|#				;shadow-filter.lisp
 |#

(in-package :cl-diagram)


;;------------------------------------------------------------------------------
;;
;; class shadow-filter
;;
;;------------------------------------------------------------------------------
(defclass shadow-filter (filter)
  ((color-matrix :initform nil :initarg :color-matrix) ; nil or list of number
   (deviation    :initform nil :initarg :deviation)    ; number
   (dx           :initform nil :initarg :dx)           ; number
   (dy           :initform nil :initarg :dy)))         ; number


(defmethod write-filter ((filt shadow-filter) writer)
  (with-slots (id color-matrix deviation dx dy) filt
	(writer-write writer "<filter id='" id "'>")
	(writer-incr-level writer)
	(let ((in nil))
	  (if color-matrix
		(writer-write writer "<feColorMatrix type='matrix' values='"
								  (format nil "~{ ~A~}" color-matrix) "' />")
		(setf in " in='SourceAlpha'"))
	  (writer-write writer "<feGaussianBlur" in " stdDeviation='" deviation "' result='blur' />")
	  (setf in " in='blur'")
	  (when dx
		(writer-write writer "<feOffset" in " dx='" dx "' dy='" dy "' result='offsetBlur' />")
		(setf in " in='offsetBlur'"))
	  (writer-write writer "<feMerge>")
	  (writer-incr-level writer)
	  (writer-write writer "<feMergeNode" in " />")
	  (writer-write writer "<feMergeNode in='SourceGraphic'/>")
	  (writer-decr-level writer)
	  (writer-write writer "</feMerge>")
	  (writer-decr-level writer)
	  (writer-write writer "</filter>"))))


;;-------------------------------------------------------------------------------
;;
;; macro drop-shadow
;;
;;-------------------------------------------------------------------------------
#|
#|EXPORT|#				:drop-shadow
 |#
(defmacro drop-shadow (&key id color-matrix deviation dx dy)
  `(register-filter (make-instance 'diagram::shadow-filter
								   :id (or ,id :drop-shadow)
								   :color-matrix (or ,color-matrix '(0 0 0 0   0
																	 0 0 0 0   0
																	 0 0 0 0   0
																	 0 0 0 0.4 0))
								   :deviation (or ,deviation 2)
								   :dx (or ,dx 4) :dy (or ,dy 4))))

;;-------------------------------------------------------------------------------
;;
;; macro glow-shadow
;;
;;-------------------------------------------------------------------------------
#|
#|EXPORT|#				:glow-shadow
 |#
(defmacro glow-shadow (&key id color-matrix deviation)
  `(register-filter (make-instance 'diagram::shadow-filter
								   :id (or ,id :glow-shadow)
								   :color-matrix (or ,color-matrix '(0 0 0 0 0
																	 0 0 0 0 0
																	 0 0 0 0 0
																	 0 0 0 1 0))
								   :deviation (or ,deviation 3))))
