#|
#|ASD|#				(:file "block-arrow"               :depends-on ("cl-diagram"
#|ASD|#																"polygon"))
#|EXPORT|#				;block-arrow.lisp
 |#

(in-package :cl-diagram)

#|
#|EXPORT|#				:*default-block-arrow-stroke*
#|EXPORT|#				:*default-block-arrow-fill*
#|EXPORT|#				:*default-block-arrow-filter*
 |#
(defparameter *default-block-arrow-stroke*       nil)
(defparameter *default-block-arrow-fill*         nil)
(defparameter *default-block-arrow-filter*       nil)

(defun make-block-arrow-points-1 (pt1 pt2 w l s)
  (let ((arrow-length (math/len2 pt1 pt2)))
	(let* ((width  (or w (/ arrow-length 10)))
		   (length (or l (/ arrow-length  6)))
		   (size   (or s (/ arrow-length  5)))
		   (sin1 (diagram::math/sin2 pt1 pt2))
		   (cos1 (diagram::math/cos2 pt1 pt2))
		   (pt3  (xy+ pt2 (* -1   length    cos1)	(* -1   length    sin1)))
		   (k1   (xy+ pt1 (* -1 (/ width 2) sin1) (*    (/ width 2) cos1)))
		   (k2   (xy+ pt1 (*    (/ width 2) sin1) (* -1 (/ width 2) cos1)))
		   (k3   (xy+ pt3 (*    (/ width 2) sin1) (* -1 (/ width 2) cos1)))
		   (k4   (xy+ pt3 (*    (/ size  2) sin1) (* -1 (/ size  2) cos1)))
		   (k5   (xy+ pt3 (* -1 (/ size  2) sin1) (*    (/ size  2) cos1)))
		   (k6   (xy+ pt3 (* -1 (/ width 2) sin1) (*    (/ width 2) cos1))))
	  `(,k1 ,k2 ,k3 ,k4 ,pt2 ,k5 ,k6))))

(defun make-block-arrow-points-2 (pt1 pt2 w l s)
  (let ((arrow-length (math/len2 pt1 pt2)))
	(let* ((width  (or w (/ arrow-length 10)))
		   (length (or l (/ arrow-length  6)))
		   (size   (or s (/ arrow-length  5)))
		   (sin1 (diagram::math/sin2 pt1 pt2))
		   (cos1 (diagram::math/cos2 pt1 pt2))
		   (pt3  (xy+ pt2 (* -1   length    cos1)	(* -1   length    sin1)))
		   (pt4  (xy+ pt1 (*      length    cos1)	(*      length    sin1)))
		   (k1   (xy+ pt4 (*    (/ size  2) sin1) (* -1 (/ size  2) cos1)))
		   (k2   (xy+ pt4 (*    (/ width 2) sin1) (* -1 (/ width 2) cos1)))
		   (k3   (xy+ pt3 (*    (/ width 2) sin1) (* -1 (/ width 2) cos1)))
		   (k4   (xy+ pt3 (*    (/ size  2) sin1) (* -1 (/ size  2) cos1)))
		   (k5   (xy+ pt3 (* -1 (/ size  2) sin1) (*    (/ size  2) cos1)))
		   (k6   (xy+ pt3 (* -1 (/ width 2) sin1) (*    (/ width 2) cos1)))
		   (k7   (xy+ pt4 (* -1 (/ width 2) sin1) (*    (/ width 2) cos1)))
		   (k8   (xy+ pt4 (* -1 (/ size  2) sin1) (*    (/ size  2) cos1))))
	  `(,pt1 ,k1 ,k2 ,k3 ,k4 ,pt2 ,k5 ,k6 ,k7 ,k8))))


;;------------------------------------------------------------------------------
;;
;; class block-arrow
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:block-arrow
 |#
(defclass block-arrow (polygon) ())
  
;;------------------------------------------------------------------------------
;;
;; macro block-arrow
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:block-arrow1
 |#
(defmacro block-arrow1 (pt1 pt2
						&key width length size fill stroke link layer filter id)
  `(register-entity (make-instance 'diagram:block-arrow
								   :points (diagram::make-block-arrow-points-1 ,pt1 ,pt2
																			   ,width ,length ,size)
								   :fill   (or ,fill   *default-block-arrow-fill*)
								   :stroke (or ,stroke *default-block-arrow-stroke*)
								   :filter (or ,filter
											   *default-block-arrow-filter*
											   *default-shape-filter*)
								   :link ,link :layer ,layer :id ,id)))

#|
#|EXPORT|#				:block-arrow2
 |#
(defmacro block-arrow2 (pt1 pt2
						&key width length size fill stroke link layer filter id)
  `(register-entity (make-instance 'diagram:block-arrow
								   :points (diagram::make-block-arrow-points-2 ,pt1 ,pt2
																			   ,width ,length ,size)
								   :fill   (or ,fill   *default-block-arrow-fill*)
								   :stroke (or ,stroke *default-block-arrow-stroke*)
								   :filter (or ,filter
											   *default-block-arrow-filter*
											   *default-shape-filter*)
								   :link ,link :layer ,layer :id ,id)))
