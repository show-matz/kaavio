#|
#|ASD|#				(:file "canvas"                    :depends-on ("cl-diagram"))
#|EXPORT|#				;canvas.lisp
 |#

(in-package :cl-diagram)

;;------------------------------------------------------------------------------
;;
;; canvas
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:canvas
 |#
;;ToDo : メンバを readonly にせよ
(defclass canvas ()
  ((top    :type     number
		   :initform 0
		   :initarg  :top
		   :accessor canvas-top)
   (bottom :type     number
		   :initform 0
		   :initarg  :bottom
		   :accessor canvas-bottom)
   (left   :type     number
		   :initform 0
		   :initarg  :left
		   :accessor canvas-left)
   (right  :type     number
		   :initform 0
		   :initarg  :right
		   :accessor canvas-right)))


#|
#|EXPORT|#				:make-canvas
 |#
(defun make-canvas (top bottom left right)
  (make-instance 'canvas :top top :bottom bottom
						 :left left :right right))

