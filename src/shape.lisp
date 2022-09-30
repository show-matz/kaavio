#|
#|ASD|#				(:file "shape"                     :depends-on ("kaavio"
#|ASD|#																"point"
#|ASD|#																"canvas"
#|ASD|#																"mathutil"
#|ASD|#																"entity"
#|ASD|#																"link-info"))
#|EXPORT|#				;shape.lisp
 |#

(in-package :kaavio)

;;------------------------------------------------------------------------------
;;
;; utility functions
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:rectangle-connect-point
 |#
(defun rectangle-connect-point-C (pt1 width height pt2)
  ;;(format t "cx=~A, cy=~A, width=~A, height=~A, pt2=~A.~%" cx cy width height pt2)
  (let* ((cx    (point-x pt1))
		 (cy    (point-y pt1))
		 (px    (point-x pt2))
		 (py    (point-y pt2))
		 (w/2   (/ width  2))
		 (h/2   (/ height 2))
		 (len1  (math/len4 cx cy (+ cx w/2) (+ cy h/2)))    ; length between center to corner.
		 (len2  (math/len4 cx cy px py))
		 (c-sin (/ h/2 len1))
		 (p-sin (/ (- py cy) len2))
		 (p-cos (/ (- px cx) len2)))
	;;(format t "c-sin=~A, p-sin=~A, p-cos=~A.~%" c-sin p-sin p-cos)
	(cond
	  ((< 0 c-sin p-sin)		;; bottom line
	   (values (make-point (+ cx (* (/ p-cos p-sin) h/2)) (+ cy h/2) :absolute) :bottom))
	  ((< p-sin (- c-sin) 0)	;;  upper line
	   (values (make-point (- cx (* (/ p-cos p-sin) h/2)) (- cy h/2) :absolute) :top))
	  ((< cx px)				;;  right line
	   (values (make-point (+ cx w/2) (+ cy (* (/ p-sin p-cos) w/2)) :absolute) :right))
	  (t						;;   left line
	   (values (make-point (- cx w/2) (- cy (* (/ p-sin p-cos) w/2)) :absolute) :left)))))

;; return point object.
(defun rectangle-connect-point (center-pt width height type1 type2 arg)
  (declare (ignore type1))
  (let ((cx (point-x center-pt))
		(cy (point-y center-pt)))
	(ecase type2
	  ((:center) (let ((pt (rectangle-connect-point-C center-pt width height arg)))
				   pt))
	  ((:top)    (make-point (+ cx (* (/ width 4) arg)) (- cy (/ height 2)) :absolute))
	  ((:bottom) (make-point (+ cx (* (/ width 4) arg)) (+ cy (/ height 2)) :absolute))
	  ((:left)   (make-point (- cx (/ width 2)) (+ cy (* (/ height 4) arg)) :absolute))
	  ((:right)  (make-point (+ cx (/ width 2)) (+ cy (* (/ height 4) arg)) :absolute)))))


;;------------------------------------------------------------------------------
;;
;; abstract class shape
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:shape
 |#
(defclass shape (entity)
  ((rotate	:initform nil :initarg :rotate)	; number
   (link	:initform nil :initarg :link)))	; (or nil link-info)

(defmethod initialize-instance :after ((shp shape) &rest initargs)
  (declare (ignore initargs))
  (with-slots (link) shp
	(setf link (make-link link)))
  shp)

(defmethod entity-composition-p ((shp shape))
  (or (slot-value shp 'link)
	  (slot-value shp 'rotate)))

#|
#|EXPORT|#				:shape-id
#|EXPORT|#				:shape-width
#|EXPORT|#				:shape-height
#|EXPORT|#				:shape-topleft
#|EXPORT|#				:shape-top
#|EXPORT|#				:shape-topright
#|EXPORT|#				:shape-left
#|EXPORT|#				:shape-center
#|EXPORT|#				:shape-right
#|EXPORT|#				:shape-bottomleft
#|EXPORT|#				:shape-bottom
#|EXPORT|#				:shape-bottomright
 |#
(defgeneric shape-id          (shp))	;; returns symbol (keyword or gensym).
(defgeneric shape-width       (shp))	;; returns number.
(defgeneric shape-height      (shp))	;; returns number.
(defgeneric shape-topleft     (shp))	;; returns point object.
(defgeneric shape-top         (shp))	;; returns point object.
(defgeneric shape-topright    (shp))	;; returns point object.
(defgeneric shape-left        (shp))	;; returns point object.
(defgeneric shape-center      (shp))	;; returns point object.
(defgeneric shape-right       (shp))	;; returns point object.
(defgeneric shape-bottomleft  (shp))	;; returns point object.
(defgeneric shape-bottom      (shp))	;; returns point object.
(defgeneric shape-bottomright (shp))	;; returns point object.

#|
#|EXPORT|#				:shape-get-subcanvas
#|EXPORT|#				:shape-cc-center
#|EXPORT|#				:shape-connect-point
 |#
(defgeneric shape-get-subcanvas (shp))	;; returns canvas object.
(defgeneric shape-cc-center (shp type)) ;; returns point object.
										;; type := :from|:dest
(defgeneric shape-connect-point (shp type1 type2 arg))
; returns point object.
; type1 := :from|:dest
; type2 := :center|:top|:bottom|:left|:right
; arg   := point object (when   (eq type :center)) |
;          -1,0,1       (unless (eq type :center))


(defmethod shape-id ((shp shape))
  (slot-value shp 'id))

;; need implement in derived class...
;;(defmethod shape-width ((shp shape)) ...)

;; need implement in derived class...
;;(defmethod shape-height ((shp shape)) ...)

(defmethod shape-topleft ((shp shape))
  (point/xy+ (shape-center shp)
			 (- (/ (shape-width  shp) 2))
			 (- (/ (shape-height shp) 2))))

(defmethod shape-top ((shp shape))
  (point/y+ (shape-center shp)
			(- (/ (shape-height shp) 2))))

(defmethod shape-topright ((shp shape))
  (point/xy+ (shape-center shp)
			 (/ (shape-width shp) 2)
			 (- (/ (shape-height shp) 2))))

(defmethod shape-left ((shp shape))
  (point/x+ (shape-center shp)
			(- (/ (shape-width shp) 2))))

;; need implement in derived class...
;;(defmethod shape-center ((shp shape)) ...)

(defmethod shape-right ((shp shape))
  (point/x+ (shape-center shp)
			(/ (shape-width shp) 2)))

(defmethod shape-bottomleft ((shp shape))
  (point/xy+ (shape-center shp)
			 (- (/ (shape-width shp) 2))
			 (/ (shape-height shp) 2)))

(defmethod shape-bottom ((shp shape))
  (point/y+ (shape-center shp)
			 (/ (shape-height shp) 2)))

(defmethod shape-bottomright ((shp shape))
  (point/xy+ (shape-center shp)
			 (/ (shape-width  shp) 2)
			 (/ (shape-height shp) 2)))



(defmethod shape-get-subcanvas ((shp shape))
  (make-canvas (shape-topleft shp)
			   (shape-width   shp)
			   (shape-height  shp)))

(defmethod shape-cc-center ((shp shape) type)
  (declare (ignore type))
  (shape-center shp))

(defmethod shape-connect-point ((shp shape) type1 type2 arg)
  (rectangle-connect-point (shape-center shp)
						   (shape-width  shp)
						   (shape-height shp) type1 type2 arg))
  

(defmethod pre-draw ((shp shape) writer)
  (when (entity-composition-p shp)
	(let ((id     (slot-value shp 'id))
		  (lnk    (slot-value shp 'link))
		  (rotate (slot-value shp 'rotate)))
	  (when (or (keywordp id) rotate)
		(let ((cc (shape-center shp)))
		  (writer-write writer "<g"
						(write-when (keywordp id) " id='" id "'")
						(write-when rotate " transform='rotate(" it ","
											(point-x cc) "," (point-y cc) ")'" )
						">"))
		(writer-incr-level writer))
	  (when lnk
		(write-link-open lnk writer)))))

(defmethod post-draw ((shp shape) writer)
  (when (entity-composition-p shp)
	(let ((id     (slot-value shp 'id))
		  (lnk    (slot-value shp 'link))
		  (rotate (slot-value shp 'rotate)))
	  (when lnk
		(write-link-close lnk writer))
	  (when (or (keywordp id) rotate)
		(writer-decr-level writer)
		(writer-write writer "</g>")))))


(defmethod check ((shp shape) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (rotate link) shp
	(check-member rotate :nullable t :types number)
	(check-object link  canvas dict :nullable t :class link-info))
  nil)
  

