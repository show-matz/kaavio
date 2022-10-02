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
#|EXPORT|#				:math/intersection-point
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


;; pt1->pt2, pt3->pt4 の両直線の交点を求めて返す
;; 平行の場合など、交点が存在しない場合は nil を返す
(defun math/intersection-point (pt1 pt2 pt3 pt4)
  (labels ((calc-coeff (xa ya xb yb)
			 (if (= xa xb)
				 :div0
				 (/ (- yb ya) (- xb xa)))))
	(let ((x1 (point-x pt1)) (y1 (point-y pt1))
		  (x2 (point-x pt2)) (y2 (point-y pt2))
		  (x3 (point-x pt3)) (y3 (point-y pt3))
		  (x4 (point-x pt4)) (y4 (point-y pt4)))
	  ;; pt1->pt2, pt3->pt4 の両直線を y=Ax+B, y=Cx+D とする
	  ;; まずは A,B,C,D を求める
	  (let* ((A (calc-coeff x1 y1 x2 y2))
			 (B (if (eq A :div0) A (- y1 (* A x1))))
			 (C (calc-coeff x3 y3 x4 y4))
			 (D (if (eq C :div0) C (- y3 (* C x3)))))
		(cond
		  ;;「交わる前提」なので、双方 :div0 はおかしい → nil 終了
		  ((and (eq A :div0) (eq C :div0)) nil)
		  ;;片方が :div0 なら他方に代入するだけで終了
		  ((eq A :div0) (make-point x1 (+ (* C x1) D)))
		  ((eq C :div0) (make-point x3 (+ (* A x3) B)))
		  ;; 交点の x,y 座標を求める : y=Ax+B, y=Cx+D より Ax+B = Cx+D => (A-C)x=(D-B)
		  ;;「交わる前提」なので、A == C もおかしい → nil 終了
		  ((= A C) nil)
		  (t (let* ((ix (/ (- D B) (- A C)))
					(iy (+ (* A ix) B)))
			   (make-point ix iy))))))))
