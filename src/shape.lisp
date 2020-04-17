#|
#|ASD|#				(:file "shape"                     :depends-on ("cl-diagram"
#|ASD|#																"canvas"
#|ASD|#																"point"
#|ASD|#																"mathutil"
#|ASD|#																"entity"
#|ASD|#																"link-info"))
#|EXPORT|#				;shape.lisp
 |#

(in-package :cl-diagram)

;-------------------------------------------------------------------------------
;
; utility functions
;
;-------------------------------------------------------------------------------
#|
#|EXPORT|#				:rectangle-connect-point
 |#
(defun rectangle-connect-point-C (cx cy width height pt)
  ;;(format t "cx=~A, cy=~A, width=~A, height=~A, pt=~A.~%" cx cy width height pt)
  (let* ((px    (point-x pt))
		 (py    (point-y pt))
		 (w/2   (/ width  2))
		 (h/2   (/ height 2))
		 (len1  (math/len4 cx cy (+ cx w/2) (+ cy h/2)))    ; length between center to corner.
		 (len2  (math/len4 cx cy px py))
		 (c-sin (/ h/2 len1))
		 (p-sin (/ (- py cy) len2))
		 (p-cos (/ (- px cx) len2)))
	;;(format t "c-sin=~A, p-sin=~A, p-cos=~A.~%" c-sin p-sin p-cos)
	(cond
	  ((< 0 c-sin p-sin)	 (make-point (+ cx (* (/ p-cos p-sin) h/2)) (+ cy h/2)))	;; bottom line
	  ((< p-sin (- c-sin) 0) (make-point (- cx (* (/ p-cos p-sin) h/2)) (- cy h/2)))	;;  upper line
	  ((< cx px)			 (make-point (+ cx w/2) (+ cy (* (/ p-sin p-cos) w/2))))	;;  right line
	  (t					 (make-point (- cx w/2) (- cy (* (/ p-sin p-cos) w/2)))))))	;;   left line

;; return point object.
(defun rectangle-connect-point (cx cy width height type arg)
  (ecase type
	((:center) (rectangle-connect-point-C cx cy width height arg))
	((:top)    (make-point (+ cx (* (/ width 4) arg)) (- cy (/ height 2))))
	((:bottom) (make-point (+ cx (* (/ width 4) arg)) (+ cy (/ height 2))))
	((:left)   (make-point (- cx (/ width 2)) (+ cy (* (/ height 4) arg))))
	((:right)  (make-point (+ cx (/ width 2)) (+ cy (* (/ height 4) arg))))))


;;------------------------------------------------------------------------------
;;
;; class shape
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:shape
 |#
(defclass shape (entity)
  ((class	;:type     keyword
			:initform nil
			:initarg  :class
			:accessor shape-class)
   (link	;:type     (or nil link-info)
			:initform nil
			:initarg  :link
			:accessor shape-link)))

(defmethod initialize-instance :after ((shp shape) &rest initargs)
  (declare (ignore initargs))
  (with-slots (link) shp
	(setf link (make-link link)))
  shp)

(defmethod entity-composition-p ((shp shape))
  (not (null (shape-link shp))))

#|
#|EXPORT|#				:shape-width
#|EXPORT|#				:shape-height
#|EXPORT|#				:shape-center
#|EXPORT|#				:shape-middle
#|EXPORT|#				:shape-top
#|EXPORT|#				:shape-bottom
#|EXPORT|#				:shape-left
#|EXPORT|#				:shape-right
#|EXPORT|#				:shape-get-subcanvas
#|EXPORT|#				:shape-connect-point
 |#
(defgeneric shape-width  (shp))
(defgeneric shape-height (shp))
(defgeneric shape-center (shp))
(defgeneric shape-middle (shp))
(defgeneric shape-top    (shp))
(defgeneric shape-bottom (shp))
(defgeneric shape-left   (shp))
(defgeneric shape-right  (shp))
(defgeneric shape-get-subcanvas (shp))

(defgeneric shape-connect-point (shp type arg))
; returns point object.
; type := :center|:top|:bottom|:left|:right
; arg  := point object (when   (eq type :center)) |
;         -1,0,1       (unless (eq type :center))


(defmethod shape-top ((shp shape))
  (- (shape-middle shp) (/ (shape-height shp) 2)))

(defmethod shape-bottom ((shp shape))
  (+ (shape-middle shp) (/ (shape-height shp) 2)))

(defmethod shape-left ((shp shape))
  (- (shape-center shp) (/ (shape-width shp) 2)))

(defmethod shape-right ((shp shape))
  (+ (shape-center shp) (/ (shape-width shp) 2)))

(defmethod shape-get-subcanvas ((shp shape))
  (make-canvas (shape-top    shp)
			   (shape-bottom shp)
			   (shape-left   shp)
			   (shape-right  shp)))

(defmethod shape-connect-point ((shp shape) type arg)
  (rectangle-connect-point (shape-center shp)
						   (shape-middle shp)
						   (shape-width  shp)
						   (shape-height shp) type arg))
  

(defmethod pre-draw ((shp shape) writer)
  (call-next-method)
  (when (entity-composition-p shp)
	(let ((lnk (shape-link shp)))
	  (when lnk
		(write-link-open lnk writer)))))

(defmethod post-draw ((shp shape) writer)
  (when (entity-composition-p shp)
	(let ((lnk (shape-link shp)))
	  (when lnk
		(write-link-close lnk writer))))
  (call-next-method))


(defmethod check ((shp shape) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (class link) shp
	(check-member class :nullable t :types (or keyword string))
	(check-object link  canvas dict :nullable t :class link-info))
  nil)
  

