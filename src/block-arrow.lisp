#|
#|ASD|#				(:file "block-arrow"               :depends-on ("kaavio"
#|ASD|#																"constants"
#|ASD|#																"polygon"))
#|EXPORT|#				;block-arrow.lisp
 |#

(in-package :kaavio)

#|
#|EXPORT|#				:*default-block-arrow-stroke*
#|EXPORT|#				:*default-block-arrow-fill*
#|EXPORT|#				:*default-block-arrow-filter*
#|EXPORT|#				:*default-block-arrow-layer*
 |#
(defparameter *default-block-arrow-stroke*       nil)
(defparameter *default-block-arrow-fill*         nil)
(defparameter *default-block-arrow-filter*       nil)
(defparameter *default-block-arrow-layer*        nil)

(defun make-block-arrow-points-1 (pt1 pt2 margin width l s)
  (let* ((pt1 (xy+ pt1 (* (kaavio::math/cos2 pt1 pt2) margin)
					   (* (kaavio::math/sin2 pt1 pt2) margin)))
		 (pt2 (xy+ pt2 (* (kaavio::math/cos2 pt2 pt1) margin)
					   (* (kaavio::math/sin2 pt2 pt1) margin)))
		 (size   (or s (* width 2)))
		 (length (or l size))
		 (sin1 (kaavio::math/sin2 pt1 pt2))
		 (cos1 (kaavio::math/cos2 pt1 pt2))
		 (pt3  (xy+ pt2 (* -1   length    cos1)	(* -1   length    sin1)))
		 (k1   (xy+ pt1 (* -1 (/ width 2) sin1) (*    (/ width 2) cos1)))
		 (k2   (xy+ pt1 (*    (/ width 2) sin1) (* -1 (/ width 2) cos1)))
		 (k3   (xy+ pt3 (*    (/ width 2) sin1) (* -1 (/ width 2) cos1)))
		 (k4   (xy+ pt3 (*    (/ size  2) sin1) (* -1 (/ size  2) cos1)))
		 (k5   (xy+ pt3 (* -1 (/ size  2) sin1) (*    (/ size  2) cos1)))
		 (k6   (xy+ pt3 (* -1 (/ width 2) sin1) (*    (/ width 2) cos1))))
	`(,k1 ,k2 ,k3 ,k4 ,pt2 ,k5 ,k6)))

(defun make-block-arrow-points-2 (pt1 pt2 margin width l s)
  (let* ((pt1 (xy+ pt1 (* (kaavio::math/cos2 pt1 pt2) margin)
					   (* (kaavio::math/sin2 pt1 pt2) margin)))
		 (pt2 (xy+ pt2 (* (kaavio::math/cos2 pt2 pt1) margin)
					   (* (kaavio::math/sin2 pt2 pt1) margin)))
		 (size   (or s (* width 2)))
		 (length (or l size))
		 (sin1 (kaavio::math/sin2 pt1 pt2))
		 (cos1 (kaavio::math/cos2 pt1 pt2))
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
	`(,pt1 ,k1 ,k2 ,k3 ,k4 ,pt2 ,k5 ,k6 ,k7 ,k8)))


;;------------------------------------------------------------------------------
;;
;; class block-arrow
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:block-arrow
 |#
(defclass block-arrow (polygon)
  ((pt1 :initform nil :initarg :pt1)	; point
   (pt2 :initform nil :initarg :pt2)))	; point
  


(defmethod attribute-center ((ent block-arrow))
  (with-slots (pt1 pt2) ent
    (make-point (/ (+ (point-x pt1) (point-x pt2)) 2)
				(/ (+ (point-y pt1) (point-y pt2)) 2))))

(defmethod attribute-end1 ((ent block-arrow))
  (slot-value ent 'pt1))

(defmethod attribute-end2 ((ent block-arrow))
  (slot-value ent 'pt2))


;;------------------------------------------------------------------------------
;;
;; macro block-arrow
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:block-arrow1
 |#
(defmacro block-arrow1 (pt1 pt2 width
						&key length size margin fill stroke link layer filter id)
  `(register-entity (make-instance 'kaavio:block-arrow
								   :pt1    ,pt1
								   :pt2    ,pt2
								   :points (kaavio::make-block-arrow-points-1
													,pt1 ,pt2 (or ,margin 0) ,width ,length ,size)
								   :fill   (or ,fill   *default-block-arrow-fill*)
								   :stroke (or ,stroke *default-block-arrow-stroke*)
								   :filter (or ,filter
											   *default-block-arrow-filter*
											   *default-filter*)
								   :layer  (or ,layer 
											   *default-block-arrow-layer*
											   *default-layer*)
								   :link ,link :id ,id)))

#|
#|EXPORT|#				:block-arrow2
 |#
(defmacro block-arrow2 (pt1 pt2 width
						&key length size margin fill stroke link layer filter id)
  `(register-entity (make-instance 'kaavio:block-arrow
								   :pt1    ,pt1
								   :pt2    ,pt2
								   :points (kaavio::make-block-arrow-points-2
													,pt1 ,pt2 (or ,margin 0) ,width ,length ,size)
								   :fill   (or ,fill   *default-block-arrow-fill*)
								   :stroke (or ,stroke *default-block-arrow-stroke*)
								   :filter (or ,filter
											   *default-block-arrow-filter*
											   *default-filter*)
								   :layer  (or ,layer 
											   *default-block-arrow-layer*
											   *default-layer*)
								   :link ,link :id ,id)))

;;------------------------------------------------------------------------------
;;
;; macro with-block-arrow-options
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:with-block-arrow-options
 |#
(defmacro with-block-arrow-options ((&key fill stroke filter layer) &rest body)
  (labels ((impl (params acc)
			 (if (null params)
				 acc
				 (let ((value  (car  params))
					   (symbol (cadr params)))
				   (impl (cddr params)
						 (if (null value)
							 acc
							 (push (list symbol value) acc)))))))
	(let ((lst (impl (list fill   '*default-block-arrow-fill*
						   stroke '*default-block-arrow-stroke*
						   filter '*default-block-arrow-filter*
						   layer  '*default-block-arrow-layer*) nil)))
	  `(let ,lst
		 ,@body))))
