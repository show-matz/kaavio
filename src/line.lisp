#|
#|ASD|#				(:file "line"                      :depends-on ("cl-diagram"
#|ASD|#																"constants"
#|ASD|#																"point"
#|ASD|#																"mathutil"
#|ASD|#																"label-info"
#|ASD|#																"stroke-info"
#|ASD|#																"endmark-info"
#|ASD|#																"entity"
#|ASD|#																"filter"
#|ASD|#																"writer"))
#|EXPORT|#				;line.lisp
 |#

(in-package :cl-diagram)


;;------------------------------------------------------------------------------
;;
;; class line
;;
;;------------------------------------------------------------------------------
(defclass line (entity)
  ((points	:initform nil :initarg :points)		; list of point
   (end1	:initform nil :initarg :end1)		; keyword
   (end2	:initform nil :initarg :end2)		; keyword
   (label	:initform nil :initarg :label)  	; (or nil label-info function)
   (stroke	:initform nil :initarg :stroke)		; (or nil stroke-info)
   (filter	:initform nil :initarg :filter)))	; (or nil keyword)


(defmethod initialize-instance :after ((ent line) &rest initargs)
  (declare (ignore initargs))
  (with-slots (end1 end2 label stroke filter) ent
	(setf end1   (make-endmark (or end1   *default-endmark-1*)))
	(setf end2   (make-endmark (or end2   *default-endmark-2*)))
	(when (and label (not (functionp label)))
	  (setf label (make-label label)))
	(setf stroke (make-stroke  (or stroke *default-stroke*)))
	(setf filter (if (eq filter :none)
					 nil
					 (or filter *default-line-filter*))))
  ent)

;; type := :from|:dest
;; returns : (cons point point)
(defun line-get-endpoints (ent type)
  (type-assert type keyword)
  (check-keywords type :from :dest)
  (let ((points (slot-value ent 'points)))
	(if (eq type :from)
		(cons (second points) (first points))
		(let ((pts points)
			  (cnt (length points)))
		  (dotimes (x (- cnt 2))
			(setf pts (cdr pts)))
		  (cons (first pts) (second pts))))))

;; returns multi-value. x, y, and sin/cos in point (x, y).
(defun line-get-center (ent)
  (labels ((make-lengths (lst acc)
			 (if (null lst)
				 (nreverse acc)
				 (let ((pt (car lst)))
				   (setf lst (cdr lst))
				   (when lst
					 (push (point-distance pt (car lst)) acc))
				   (make-lengths lst acc)))))
	(let ((points (slot-value ent 'points)))
	  (unless (<= 2 (length points))
		(throw-exception "Can't get center of line."))
	  (let* ((lengths (make-lengths points nil))
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
				  (math/cos2 pt1 pt2)))))))



(defmethod check ((ent line) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (points end1 end2 label stroke filter) ent
	(check-member points :nullable nil :types list)
	(check-object end1   canvas dict :nullable   t :class endmark-info)
	(check-object end2   canvas dict :nullable   t :class endmark-info)
	(unless (functionp label)
	  (check-object label canvas dict :nullable t :class label-info))
	(check-object stroke canvas dict :nullable nil :class  stroke-info)
	(check-member filter :nullable   t :types keyword)
	(when end1 (check end1 canvas dict))
	(when end2 (check end2 canvas dict))
	(unless (<= 2 (length points))
	  (throw-exception "Less than 2 elements in points of line."))
	(labels ((fix-points (lst acc)
			   (if (null lst)
				   (nreverse acc)
				   (let ((pt (car lst)))
					 (unless (point-p pt)
					   (throw-exception "Invalid point '~A' in points of line." pt))
					 (fix-points (cdr lst)
								 (push (canvas-fix-point canvas pt) acc))))))
	  (setf points (fix-points points nil))))
  nil)
 
(defmethod entity-composition-p ((ent line))
  (or (slot-value ent 'end1)
	  (slot-value ent 'end2)
	  (slot-value ent 'label)))

(defmethod draw-entity ((ent line) writer)
  (with-slots (points label end1 end2 stroke filter) ent
	(let ((id  (and (not (entity-composition-p ent))
					(slot-value ent 'id))))
	  (pre-draw ent writer)
	  (labels ((format-points (pts st)
				 (when pts
				   (let ((pt (car pts)))
					 (format st " ~A,~A"
							 (coerce (point-x pt) 'single-float)
							 (coerce (point-y pt) 'single-float)))
				   (format-points (cdr pts) st))))
		(writer-write writer
					  "<polyline "
					  (write-when id "id='" it "' ")
					  "fill='none' "
					  (when stroke
						(to-property-strings stroke))
					  "points='" (with-output-to-string (st)
								   (format-points points st)) "' "
					  (write-when filter "filter='url(#" it ")' ")
					  "/>"))
	  (when label
		(multiple-value-bind (x y sin cos) (line-get-center ent)
		  (if (functionp label)
			  (funcall label           ent x y sin cos writer)
			  (draw-label-with-point label x y sin cos writer))))
	  (when end1
		(draw-endmark end1 (line-get-endpoints ent :from) stroke writer))
	  (when end2
		(draw-endmark end2 (line-get-endpoints ent :dest) stroke writer))
	  (post-draw ent writer)))
  nil)
  

;;------------------------------------------------------------------------------
;;
;; macro line
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:line
 |#
(defmacro line (points &key stroke label end1 end2 layer filter id)
  `(register-entity (make-instance 'diagram:line
								   :points ,points
								   :end1 ,end1 :end2 ,end2 :label ,label
								   :stroke ,stroke :filter ,filter
								   :layer ,layer :id ,id)))

