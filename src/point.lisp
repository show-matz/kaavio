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
 |#
(defun make-point (x y &optional type)
  (let ((type (or type :relative)))
	(cond
	  ((eq type :relative) (cons x (cons y nil)))
	  ((eq type :absolute) (cons x (cons y :absolute)))
	  (t (error "type must be :relative or :absolute.")))))

(defun copy-point (pt)
  (make-point (car pt) (cadr pt) (cddr pt)))

#|
#|EXPORT|#				:point-p
#|EXPORT|#				:point-absolute-p
#|EXPORT|#				:point-relative-p
 |#
(defun point-p (pt)
  (and (consp pt)
	   (consp   (cdr  pt))
	   (numberp (car  pt))
	   (numberp (cadr pt))))

(defun point-absolute-p (pt)
  (eq (cddr pt) :absolute))

(defun point-relative-p (pt)
  (null (cddr pt)))

#|
#|EXPORT|#				:point-x
#|EXPORT|#				:point-y
 |#
(defun point-x (pt) (car  pt))
(defun point-y (pt) (cadr pt))
(defun (setf point-x) (val pt) (setf (car  pt) val))
(defun (setf point-y) (val pt) (setf (cadr pt) val))

#|
#|EXPORT|#				:point+
#|EXPORT|#				:point-
#|EXPORT|#				:point/x+
#|EXPORT|#				:point/y+
#|EXPORT|#				:point/xy+
#|EXPORT|#				:x+
#|EXPORT|#				:y+
#|EXPORT|#				:xy+
 |#
(defun point+ (pt1 pt2)
  (make-point (+  (car  pt1) (car  pt2))
			  (+  (cadr pt1) (cadr pt2))
			  (or (cddr pt1) (cddr pt2))))

(defun point- (pt1 pt2)
  (make-point (-  (car  pt1) (car  pt2))
			  (-  (cadr pt1) (cadr pt2))
			  (or (cddr pt1) (cddr pt2))))

(defun point/x+ (pt x)
  (make-point (+ (car pt) x) (cadr pt) (cddr pt)))

(defun point/y+ (pt y)
  (make-point (car pt) (+ (cadr pt) y) (cddr pt)))

(defun point/xy+ (pt x y)
  (make-point (+ (car  pt) x)
			  (+ (cadr pt) y) (cddr pt)))

(defun x+ (pt x)
  (make-point (+ (car pt) x) (cadr pt) (cddr pt)))

(defun y+ (pt y)
  (make-point (car pt) (+ (cadr pt) y) (cddr pt)))

(defun xy+ (pt x y)
  (make-point (+ (car  pt) x)
			  (+ (cadr pt) y) (cddr pt)))

#|
#|EXPORT|#				:point-distance
 |#
(defun point-distance (pt1 pt2)
  (let ((x (- (point-x pt1) (point-x pt2)))
		(y (- (point-y pt1) (point-y pt2))))
	(sqrt (+ (* x x) (* y y)))))

#|
#|EXPORT|#				:with-point
 |#
(defmacro with-point ((sym-x sym-y) pt &rest body)
  (let ((g-pt (gensym "PT")))
	`(let ((,g-pt ,pt))
	   (symbol-macrolet ((,sym-x (car  ,g-pt))
						 (,sym-y (cadr ,g-pt)))
		 (declare (ignorable ,sym-x ,sym-y))
		 ,@body))))

