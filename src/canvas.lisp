#|
#|ASD|#				(:file "canvas"                    :depends-on ("cl-diagram"
#|ASD|#																"point"))
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
#|EXPORT|#				:make-canvas
#|EXPORT|#				:copy-canvas
#|EXPORT|#				:canvas-p
 |#
(defun make-canvas (top-left width height)
  (cons top-left (cons width height)))

(defun copy-canvas (canvas)
  (copy-tree canvas))

(defun canvas-p (canvas)
  (and (consp         canvas)
	   (point-p (car  canvas))
	   (numberp (cadr canvas))
	   (numberp (cddr canvas))))

#|
#|EXPORT|#				:canvas-topleft
#|EXPORT|#				:canvas-left
#|EXPORT|#				:canvas-top
#|EXPORT|#				:canvas-right
#|EXPORT|#				:canvas-bottom
 |#
(defun canvas-topleft (canvas) (car canvas))
(defun canvas-left    (canvas) (point-x (car canvas)))
(defun canvas-top     (canvas) (point-y (car canvas)))
(defun (setf canvas-left) (val canvas) (setf (point-x (car canvas)) val))
(defun (setf canvas-top)  (val canvas) (setf (point-y (car canvas)) val))
(defun canvas-right   (canvas) (+ (point-x (car canvas)) (cadr canvas)))
(defun canvas-bottom  (canvas) (+ (point-y (car canvas)) (cddr canvas)))

#|
#|EXPORT|#				:canvas-width
#|EXPORT|#				:canvas-height
 |#
(defun canvas-width  (canvas) (cadr canvas))
(defun canvas-height (canvas) (cddr canvas))
(defun (setf canvas-width)  (val canvas) (setf (cadr canvas) val))
(defun (setf canvas-height) (val canvas) (setf (cddr canvas) val))

#|
#|EXPORT|#				:canvas-fix-point
 |#
(defun canvas-fix-point (canvas pt)
  (if (point-absolute-p pt)
	  pt
	  (point+ (canvas-topleft canvas) pt)))

#|
#|EXPORT|#				:with-canvas
 |#
(defmacro with-canvas ((sym-topleft sym-width sym-height) canvas &rest body)
  (let ((g-canvas (gensym "CANVAS")))
	`(let ((,g-canvas ,canvas))
	   (symbol-macrolet ((,sym-topleft (car  ,g-canvas))
						 (,sym-width   (cadr ,g-canvas))
						 (,sym-height  (cddr ,g-canvas)))
		 ,@body))))

#|
#|EXPORT|#				:with-subcanvas
 |#
(defmacro with-subcanvas ((top-left width height) &rest body)
  `(let ((canvas (make-canvas (point+ (car canvas) ,top-left) ,width ,height)))
	 (declare (special canvas))
	 ,@body))






;; for expansion in 'with-dictionary'
(defun canvas-dict-width       (canvas) (cadr canvas))
(defun canvas-dict-height      (canvas) (cddr canvas))
(defun canvas-dict-topleft     (canvas) (car  canvas))
(defun canvas-dict-top         (canvas) (point/x+  (car canvas) (/ (cadr canvas) 2)))
(defun canvas-dict-topright    (canvas) (point/x+  (car canvas)    (cadr canvas)))
(defun canvas-dict-left        (canvas) (point/y+  (car canvas) (/ (cddr canvas) 2)))
(defun canvas-dict-center      (canvas) (point/xy+ (car canvas) (/ (cadr canvas) 2) (/ (cddr canvas) 2)))
(defun canvas-dict-right       (canvas) (point/xy+ (car canvas)    (cadr canvas)    (/ (cddr canvas) 2)))
(defun canvas-dict-bottomleft  (canvas) (point/y+  (car canvas)    (cddr canvas)))
(defun canvas-dict-bottom      (canvas) (point/xy+ (car canvas) (/ (cadr canvas) 2)    (cddr canvas)))
(defun canvas-dict-bottomright (canvas) (point/xy+ (car canvas)    (cadr canvas)       (cddr canvas)))
