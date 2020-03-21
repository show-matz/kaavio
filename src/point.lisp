#|
#|ASD|#				(:file "point"                     :depends-on ("cl-diagram"))
#|EXPORT|#				;point.lisp
 |#

(in-package :cl-diagram)

;;------------------------------------------------------------------------------
;;
;; point
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:make-point
#|EXPORT|#				:copy-point
#|EXPORT|#				:point-x
#|EXPORT|#				:point-y
#|EXPORT|#				:point-distance
#|EXPORT|#				:point-offset
 |#
(defun make-point (&optional x y)
  (let ((x (or x 0))
		(y (or y 0)))
	(cons x y)))

(defun copy-point (pt)
  (make-point (car pt) (cdr pt)))

(defun point-x (pt)
  (car pt))

(defun point-y (pt)
  (cdr pt))

(defun point-distance (pt1 pt2)
  (let ((x (- (point-x pt1) (point-x pt2)))
		(y (- (point-y pt1) (point-y pt2))))
	(sqrt (+ (* x x) (* y y)))))

(defun point-offset (pt x y)
  (make-point (+ (point-x pt) x) (+ (point-y pt) y)))


