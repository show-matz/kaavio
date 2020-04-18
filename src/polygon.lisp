#|
#|ASD|#				(:file "polygon"                   :depends-on ("cl-diagram"
#|ASD|#																"constants"
#|ASD|#																"fill-info"
#|ASD|#																"stroke-info"
#|ASD|#																"link-info"
#|ASD|#																"entity"
#|ASD|#																"writer"))
#|EXPORT|#				;polygon.lisp
 |#

(in-package :cl-diagram)

;-------------------------------------------------------------------------------
;
; class polygon
;
;-------------------------------------------------------------------------------
(defclass polygon (entity)
  ((points	;:type     list
			:initform nil
			:initarg  :points
			:accessor polygon-points)
   (class	;:type     keyword
			:initform nil
			:initarg  :class
			:accessor polygon-class)
   (fill	;:type     (or nil fill-info)
			:initform nil
			:initarg  :fill
			:accessor polygon-fill)
   (stroke	;:type     (or nil stroke-info)
			:initform nil
			:initarg  :stroke
			:accessor polygon-stroke)
   (link	;:type     (or nil link-info)
			:initform nil
			:initarg  :link
			:accessor polygon-link)))


(defmethod initialize-instance :after ((ent polygon) &rest initargs)
  (declare (ignore initargs))
  (with-slots (points fill stroke link) ent
	(setf points (copy-list points))
	(setf fill   (make-fill   (or fill   *default-fill*)))
	(setf stroke (make-stroke (or stroke *default-stroke*)))
	(setf link   (make-link   link)))
  ent)
  
(defmethod check ((ent polygon) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (points class fill stroke link) ent
	(check-member points :nullable nil :types list)
	(check-member class  :nullable   t :types (or keyword string))
	(check-object fill   canvas dict :nullable nil :class fill-info)
	(check-object stroke canvas dict :nullable nil :class stroke-info)
	(check-object link   canvas dict :nullable   t :class link-info)
	(unless (evenp (length points))
	  (throw-exception "Odd number elements in points of polygon."))
	(unless (<= 6 (length points))
	  (throw-exception "Less than 6 elements in points of polygon."))
	(dolist (v points)
	  (unless (numberp v)
		(throw-exception "Invalid value '~A' in points of polygon." v)))
	(let ((x (canvas-left canvas))
		  (y (canvas-top  canvas)))
	  (do ((lst points (cddr lst)))
		  ((null lst))
		(incf (car  lst) x)
		(incf (cadr lst) y))))
  nil)
	
(defmethod entity-composition-p ((ent polygon))
  (not (null (polygon-link ent))))

(defmethod pre-draw ((ent polygon) writer)
  (call-next-method)
  (when (entity-composition-p ent)
	(let ((lnk (polygon-link ent)))
	  (when lnk
		(write-link-open lnk writer)))))

(defmethod post-draw ((ent polygon) writer)
  (when (entity-composition-p ent)
	(let ((lnk (polygon-link ent)))
	  (when lnk
		(write-link-close lnk writer))))
  (call-next-method))

(defmethod draw-entity ((ent polygon) writer)
  (labels ((format-points (pts)
			 (with-output-to-string (stream)
			   (do ((idx 0 (incf idx)))
				   ((null pts) nil)
				 (unless (zerop idx)
				   (princ #\space stream))
				 (format stream "~A,~A" (coerce (car  pts) 'single-float)
										(coerce (cadr pts) 'single-float))
				 (setf pts (cddr pts))))))
  (let ((cls (polygon-class ent))
		(id  (and (not (entity-composition-p ent))
				  (slot-value ent 'id))))
	(pre-draw ent writer)
	(writer-write writer
				  "<polygon "
				  (write-when id     "id='" it "' ")
				  (write-when cls "class='" it "' ")
				  (unless cls
					(let ((fill (polygon-fill ent)))
					  (when fill
						(to-property-strings fill))))
				  (unless cls
					(let ((strk (polygon-stroke ent)))
					  (when strk
						(to-property-strings strk))))
				  "points='" (format-points (polygon-points ent)) "' "
				  "/>")
	(pre-draw ent writer))))


;;   (:public set-points (points)
;;		(type-assert points list)
;;		(dolist (v points)
;;		  (type-assert v number))
;;		(setf m-points (copy-list points)))
;;


#|
#|EXPORT|#				:polygon
 |#
(defmacro polygon (points &key class fill stroke link layer id)
  `(register-entity (make-instance 'diagram:polygon
								   :points ,points :class ,class
								   :fill ,fill :stroke ,stroke
								   :link ,link :layer ,layer :id ,id)))

