#|
#|ASD|#				(:file "mathutil"                  :depends-on ("kaavio"
#|ASD|#																"point"))
#|EXPORT|#				;mathutil.lisp
 |#

(in-package :kaavio)

;;------------------------------------------------------------------------------
;;
;; mathutil
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:math/len2
#|EXPORT|#				:math/len4
#|EXPORT|#				:math/sin1
#|EXPORT|#				:math/sin2
#|EXPORT|#				:math/sin3
#|EXPORT|#				:math/sin4
#|EXPORT|#				:math/sin5
#|EXPORT|#				:math/cos1
#|EXPORT|#				:math/cos2
#|EXPORT|#				:math/cos3
#|EXPORT|#				:math/cos4
#|EXPORT|#				:math/cos5
 |#
(symbol-macrolet ((RADIAN-UNIT (/ pi 180)))

  ;;--------------------------------------------------------
  ;; math/len
  ;;--------------------------------------------------------
  (defun math/len4 (x1 y1 x2 y2)
	(let ((w (- x2 x1))
		  (h (- y2 y1)))
	  (sqrt (+ (* w w) (* h h)))))

  (defun math/len2 (pt1 pt2)
	(math/len4 (point-x pt1)
			   (point-y pt1)
			   (point-x pt2)
			   (point-y pt2)))

  ;;--------------------------------------------------------
  ;; math/sin & math/cos
  ;;--------------------------------------------------------
  (defun math/sin1 (degree)
	(unless (and (<= 0 degree) (< degree 360))
	  (throw-exception "Degree for math/sin must be [0, 360)."))
	(if (= 180 degree)
		0
		(cl:sin (* RADIAN-UNIT degree))))

  (defun math/cos1 (degree)
	(unless (and (<= 0 degree) (< degree 360))
	  (throw-exception "Degree for math/sin must be [0, 360)."))
	(if (or (= 90 degree) (= 270 degree))
		0
		(cl:cos (* RADIAN-UNIT degree))))


  (defun math/sin2 (pt1 pt2)
	(/ (- (point-y pt2)
		  (point-y pt1)) (math/len2 pt1 pt2)))

  (defun math/cos2 (pt1 pt2)
	(/ (- (point-x pt2)
		  (point-x pt1)) (math/len2 pt1 pt2)))


  (defun math/sin3 (pt1 pt2 degree)
	(+ (* (math/sin2 pt1 pt2) (math/cos1 degree))
	   (* (math/cos2 pt1 pt2) (math/sin1 degree))))

  (defun math/cos3 (pt1 pt2 degree)
	(- (* (math/cos2 pt1 pt2) (math/cos1 degree))
	   (* (math/sin2 pt1 pt2) (math/sin1 degree))))


  (defun math/sin4 (x1 y1 x2 y2)
	(/ (- y2 y1) (math/len4 x1 y1 x2 y2)))

  (defun math/cos4 (x1 y1 x2 y2)
	(/ (- x2 x1) (math/len4 x1 y1 x2 y2)))


  (defun math/sin5 (x1 y1 x2 y2 degree)
	(+ (* (math/sin4 x1 y1 x2 y2) (math/cos1 degree))
	   (* (math/cos4 x1 y1 x2 y2) (math/sin1 degree))))

  (defun math/cos5 (x1 y1 x2 y2 degree)
	(- (* (math/cos4 x1 y1 x2 y2) (math/cos1 degree))
	   (* (math/sin4 x1 y1 x2 y2) (math/sin1 degree)))))

