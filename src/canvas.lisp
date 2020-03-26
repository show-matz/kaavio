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
#|EXPORT|#				:make-canvas
 |#
(defun make-canvas (top bottom left right)
  (cons (cons top left) (cons bottom right)))

#|
#|EXPORT|#				:canvas-top
#|EXPORT|#				:canvas-bottom
#|EXPORT|#				:canvas-left
#|EXPORT|#				:canvas-right
 |#
(defun canvas-top    (canvas) (caar canvas))
(defun canvas-bottom (canvas) (cadr canvas))
(defun canvas-left   (canvas) (cdar canvas))
(defun canvas-right  (canvas) (cddr canvas))
(defun (setf canvas-top)    (val canvas) (setf (caar canvas) val))
(defun (setf canvas-bottom) (val canvas) (setf (cadr canvas) val))
(defun (setf canvas-left)   (val canvas) (setf (cdar canvas) val))
(defun (setf canvas-right)  (val canvas) (setf (cddr canvas) val))

#|
#|EXPORT|#				:canvas-width
#|EXPORT|#				:canvas-height
 |#
(defun canvas-width  (canvas) (- (cddr canvas) (cdar canvas)))
(defun canvas-height (canvas) (- (cadr canvas) (caar canvas)))

#|
#|EXPORT|#				:with-canvas
 |#
(defmacro with-canvas ((t-sym b-sym l-sym r-sym) canvas &rest body)
  (let ((g-canvas (gensym "CANVAS")))
	`(let ((,g-canvas ,canvas))
	   (symbol-macrolet ((,t-sym (caar ,g-canvas))
						 (,b-sym (cadr ,g-canvas))
						 (,l-sym (cdar ,g-canvas))
						 (,r-sym (cddr ,g-canvas)))
		 ,@body))))

