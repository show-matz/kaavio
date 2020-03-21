#|
#|ASD|#				(:file "line"                      :depends-on ("cl-diagram"
#|ASD|#																"constants"
#|ASD|#																"point"
#|ASD|#																"mathutil"
#|ASD|#																"stroke-info"
#|ASD|#																"endmark-info"
#|ASD|#																"entity"
#|ASD|#																"writer"))
#|EXPORT|#				;line.lisp
 |#

(in-package :cl-diagram)


;-------------------------------------------------------------------------------
;
; utility functions
;
;-------------------------------------------------------------------------------
(defun __get-line-center-imp (lst)
  (labels ((make-points (lst acc)
			 (if (null lst)
				 (nreverse acc)
				 (make-points (cddr lst)
							  (push (make-point (car  lst)
												(cadr lst)) acc))))
		   (make-lengths (lst acc)
			 (if (null lst)
				 (nreverse acc)
				 (let ((pt (car lst)))
				   (setf lst (cdr lst))
				   (when lst
					 (push (point-distance pt (car lst)) acc))
				   (make-lengths lst acc)))))
	(unless (and (<= 4 (length lst))
				 (evenp (length lst)))
	  (throw-exception "Can't get center of line."))
	(let* ((points  (make-points lst nil))
		   (lengths (make-lengths points nil))
		   (half    (/ (apply #'+ lengths) 2))
		   (acc (do ((acc (car lengths)
						  (+ acc (car lengths))))
					((< half acc) acc)
				  (setf points  (cdr points))
				  (setf lengths (cdr lengths))))
		   (prev (- acc (car lengths)))
		   (ratio (/ (- half prev) (- acc prev))))
#|
#|DBG|#	(dolist (pt points)
#|DBG|#		(class:with-access pt
#|DBG|#		  (format t "(~A, ~A).~%" pt.x pt.y)))
#|DBG|#	(format t "lengths : ~A~%" lengths)
#|DBG|#	(format t "half    : ~A~%" half)
#|DBG|#	(format t "prev    : ~A~%" prev)
#|DBG|#	(format t "acc     : ~A~%" acc)
#|DBG|#	(format t "ratio   : ~A~%" ratio)
|#
		(let ((pt1 (car  points))
			  (pt2 (cadr points)))
		  (values (+ (point-x pt1) (* ratio (- (point-x pt2) (point-x pt1))))
				  (+ (point-y pt1) (* ratio (- (point-y pt2) (point-y pt1))))
				  (math/sin2 pt1 pt2)
				  (math/cos2 pt1 pt2))))))


;-------------------------------------------------------------------------------
;
; class line
;
;-------------------------------------------------------------------------------
(defclass line (entity)
  ((points		:type     list
				:initform nil
				:initarg  :points
				:accessor line-points)
   (class		;:type     keyword
				:initform nil
				:initarg  :class
				:accessor line-class)
   (end1		;:type     keyword
				:initform nil
				:initarg  :end1
				:accessor line-end1)
   (end2		;:type     keyword
				:initform nil
				:initarg  :end2
				:accessor line-end2)
   (stroke		;:type     (or nil stroke-info)
				:initform nil
				:initarg  :stroke
				:accessor line-stroke)))


(defmethod initialize-instance :after ((ent line) &rest initargs)
  (declare (ignore initargs))
  (with-slots (end1 end2 stroke) ent
	(setf end1   (make-endmark (or end1   *default-endmark-1*)))
	(setf end2   (make-endmark (or end2   *default-endmark-2*)))
	(setf stroke (make-stroke  (or stroke *default-stroke*))))
  ent)


;
;;   (:public get-points ()
;;		(copy-list m-points))
;;
;;   (:public set-points (points)
;;		(type-assert points list)
;;		(dolist (v points)
;;		  (type-assert v number))
;;		(setf m-points (copy-list points)))
;;

;; type := :from|:dest
;; returns : (cons point point)
(defun line-get-endpoints (ent type)
  (type-assert type keyword)
  (check-keywords (type type) :from :dest)
  (let ((points (line-points ent)))
	(if (eq type :from)
		(cons (make-point (third points) (fourth points))
			  (make-point (first points) (second points)))
		(let ((pts points)
			  (cnt (/ (length points) 2)))
		  (dotimes (x (- cnt 2))
			(setf pts (cddr pts)))
		  (cons (make-point (first pts) (second pts))
				(make-point (third pts) (fourth pts)))))))

;; returns multi-value. x, y, and sin/cos in point (x, y).
(defun line-get-center (ent)
  (__get-line-center-imp (line-points ent)))



(defmethod check ((ent line) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (check-member (points (line-points ent)) :nullable nil :types list)
  (check-member (class  (line-class  ent)) :nullable   t :types (or keyword string))
  (check-object (end1   (line-end1   ent)) canvas dict :nullable   t :class endmark-info)
  (check-object (end2   (line-end2   ent)) canvas dict :nullable   t :class endmark-info)
  (check-object (stroke (line-stroke ent)) canvas dict :nullable nil :class  stroke-info)
  (when (line-end1 ent) (check (line-end1 ent) canvas dict))
  (when (line-end2 ent) (check (line-end2 ent) canvas dict))
  (let ((points (line-points ent)))
	(unless (evenp (length points))
	  (throw-exception "Odd number elements in points of line."))
	(unless (<= 4 (length points))
	  (throw-exception "Less than 4 elements in points of line."))
	(dolist (v points)
	  (unless (numberp v)
		(throw-exception "Invalid value '~A' in points of line." v)))
	(let ((x (canvas-left canvas))
		  (y (canvas-top  canvas)))
	  (do ((lst points (cddr lst)))
		  ((null lst))
		(incf (car  lst) x)
		(incf (cadr lst) y))))
  nil)

(defmethod entity-composition-p ((ent line))
  (or (line-end1 ent)
	  (line-end2 ent)))

(defmethod draw-entity ((ent line) writer)
  (let ((cls (line-class ent))
		(id  (and (not (entity-composition-p ent))
				  (entity-id ent))))
	(pre-draw ent writer)
	(labels ((format-points (pts st)
			   (when pts
				 (format st " ~A,~A" (coerce (car  pts) 'single-float)
									 (coerce (cadr pts) 'single-float))
				 (format-points (cddr pts) st))))
	  (writer-write writer
					"<polyline "
					(write-when id "id='" it "' ")
					"fill='none' "
					(write-when cls "class='" it "' ")
					(unless cls
					  (let ((strk (line-stroke ent)))
						(when strk
						  (to-property-strings strk))))
					"points='" (with-output-to-string (st)
								 (format-points (line-points ent) st)) "' "
					"/>"))
	(let ((end1 (line-end1 ent))
		  (end2 (line-end2 ent)))
	  (when end1
		(draw-endmark end1 (line-get-endpoints ent :from) cls (line-stroke ent) writer))
	  (when end2
		(draw-endmark end2 (line-get-endpoints ent :dest) cls (line-stroke ent) writer)))
	(post-draw ent writer))
  nil)
  

#|
#|EXPORT|#				:line
 |#
(defmacro line (points &key class stroke end1 end2 layer id)
  `(register-entity (make-instance 'diagram:line
								   :points ,points :class ,class
								   :end1 ,end1 :end2 ,end2
								   :stroke ,stroke :layer ,layer :id ,id)))

