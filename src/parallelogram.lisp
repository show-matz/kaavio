#|
#|ASD|#				(:file "parallelogram"             :depends-on ("kaavio"
#|ASD|#																"mathutil"
#|ASD|#																"constants"
#|ASD|#																"canvas"
#|ASD|#																"point"
#|ASD|#																"shape"
#|ASD|#																"stroke-info"
#|ASD|#																"link-info"
#|ASD|#																"filter"
#|ASD|#																"writer"))
#|EXPORT|#				;parallelogram.lisp
 |#


(in-package :kaavio)

(defun parallelogram-get-vertex (w h dir offs)
  (cond
    ((and (eq dir :h) (<= 0 offs))
     (values #|x1|# (- (/ w 2) offs)                 ;;   (x3 y3)    |         (x4 y4)
             #|y1|# (/ h 2)                          ;;         *----|--------*
             #|x2|# (- (/ w 2))                      ;;        /     |       /
             #|y2|# (/ h 2)                          ;;    ---/------+------/---
             #|x3|# (- offs (/ w 2))                 ;;      /       |     /
             #|y3|# (- (/ h 2))                      ;;     *--------|----*
             #|x4|# (/ w 2)                          ;;   (x2 y2)    |     (x1 y1)
             #|y4|# (- (/ h 2))))
    ((and (eq dir :h) (< offs 0))
     (values #|x1|# (/ w 2)                          ;; (x3 y3)      |    (x4 y4)
             #|y1|# (/ h 2)                          ;;     *--------|----*
             #|x2|# (- (abs offs) (/ w 2))           ;;      \       |     \
             #|y2|# (/ h 2)                          ;;    ---\------+------\---
             #|x3|# (- (/ w 2))                      ;;        \     |       \
             #|y3|# (- (/ h 2))                      ;;         *----|--------*
             #|x4|# (- (/ w 2) (abs offs))           ;;     (x2 y2)  |        (x1 y1)
             #|y4|# (- (/ h 2))))
    ((and (eq dir :v) (<= 0 offs))
     (values #|x1|# (/ w 2)                          ;;   (x3 y3)    |------* (x4 y4)  
             #|y1|# (- (/ h 2) offs)                 ;;       *------|      |
             #|x2|# (- (/ w 2))                      ;;       |      |      |
             #|y2|# (/ h 2)                          ;;    ---|------+------|---
             #|x3|# (- (/ w 2))                      ;;       |      |      |
             #|y3|# (- offs (/ h 2))                 ;;       |      |------*
             #|x4|# (/ w 2)                          ;;(x2 y2)*------|       (x1 y1)
             #|y4|# (- (/ h 2))))
    ((and (eq dir :v) (< offs 0))
     (values #|x1|# (/ w 2)                          ;;(x3 y3)*------|    (x4 y4)
             #|y1|# (/ h 2)                          ;;       |      |------*
             #|x2|# (- (/ w 2))                      ;;       |      |      |
             #|y2|# (+ (/ h 2) offs)                 ;;    ---|------+------|---
             #|x3|# (- (/ w 2))                      ;;       |      |      |
             #|y3|# (- (/ h 2))                      ;;       *------|      |
             #|x4|# (/ w 2)                          ;; (x2 y2)      |------*(x1 y1)
             #|y4|# (- (abs offs) (/ h 2))))))

;;------------------------------------------------------------------------------
;;
;; utility functions
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:parallelogram-connect-point
 |#
(defun parallelogram-connect-point-C (cx cy w h dir offs pt)
  (let ((pt (xy+ pt (- cx) (- cy))))
	(multiple-value-bind (x1 y1 x2 y2 x3 y3 x4 y4)
							(parallelogram-get-vertex w h dir offs)
	  (labels ((impl (pt1 pt2 pt3 pt4 check)
				 (let ((cand (math/intersection-point '(0 0) pt pt1 pt2)))
				   (if (and cand (funcall check cand))
					   cand
					   (math/intersection-point '(0 0) pt pt3 pt4)))))
		(with-point (px py) pt
		  (let ((ret (cond
					   ((and (eq dir :h) (<= 0 px) (<= 0 py))
						(impl `(0 ,y1) `(,x1 ,y1) `(,x1 ,y1) `(,(/ (+ x1 x4) 2) 0)
							  (lambda (pt) (<= 0 (point-x pt) x1))))
					   ((and (eq dir :h) (< px  0) (<= 0 py))
						(impl `(,x2 ,y2) `(0 ,y2) `(,x2 ,y2) `(,(/ (+ x2 x3) 2) 0)
							  (lambda (pt) (<= x2 (point-x pt) 0))))
					   ((and (eq dir :h) (< px  0) (< py  0))
						(impl `(,x3 ,y3) `(0 ,y3) `(,(/ (+ x2 x3) 2) 0) `(,x3 ,y3)
							  (lambda (pt) (<= x3 (point-x pt) 0))))
					   ((and (eq dir :h) (<= 0 px) (< py  0))
						(impl `(0 ,y4) `(,x4 ,y4) `(,(/ (+ x1 x4) 2) 0) `(,x4 ,y4)
							  (lambda (pt) (<= 0 (point-x pt) x4))))
					   ((and (eq dir :v) (<= 0 px) (<= 0 py))
						(impl `(0 ,(/ (+ y1 y2) 2)) `(,x1 ,y1) `(,x1 0) `(,x1 ,y1)
							  (lambda (pt) (<= 0 (point-x pt) x1))))
					   ((and (eq dir :v) (< px  0) (<= 0 py))
						(impl `(,x2 ,y2) `(0 ,(/ (+ y1 y2) 2)) `(,x2 0) `(,x2 ,y2)
							  (lambda (pt) (<= x2 (point-x pt) 0))))
					   ((and (eq dir :v) (< px  0) (< py  0))
						(impl `(,x3 ,y3) `(0 ,(/ (+ y3 y4) 2)) `(,x3 ,y3) `(,x3 0)
							  (lambda (pt) (<= x3 (point-x pt) 0))))
					   ((and (eq dir :v) (<= 0 px) (< py  0))
						(impl `(0 ,(/ (+ y3 y4) 2)) `(,x4 ,y4) `(,x4 ,y4) `(,x1 0)
							  (lambda (pt) (<= 0 (point-x pt) x4)))))))
			(make-point (+ (point-x ret) cx)
						(+ (point-y ret) cy) :absolute)))))))

(defun parallelogram-connect-point-T (cx cy w h dir offs idx)
  (if (eq dir :h)
	  (let ((x (+ cx (/ offs 2)))
			(y (- cy (/ h 2)))
			(w (- w (abs offs))))
		(make-point (+ x (* idx (/ w 4))) y :absolute))
	  (let ((x (+ cx (* idx (/ w 4))))
			(y (- cy (/ h 2) (- (/ (abs offs) 2)))))
		(make-point x (- y (* idx (/ offs 4))) :absolute))))

(defun parallelogram-connect-point-B (cx cy w h dir offs idx)
  (if (eq dir :h)
	  (let ((x (- cx (/ offs 2)))
			(y (+ cy (/ h 2)))
			(w (- w (abs offs))))
		(make-point (+ x (* idx (/ w 4))) y :absolute))
	  (let ((x (+ cx (* idx (/ w 4))))
			(y (+ cy (/ h 2) (- (/ (abs offs) 2)))))
		(make-point x (- y (* idx (/ offs 4))) :absolute))))

(defun parallelogram-connect-point-L (cx cy w h dir offs idx)
  (if (eq dir :v)
	  (let ((x (- cx (/ w 2)))
			(y (+ cy (/ offs 2)))
			(h (- h (abs offs))))
		(make-point x (+ y (* idx (/ h 4))) :absolute))
	  (let ((x (- cx (/ w 2) (- (/ (abs offs) 2))))
			(y (+ cy (* idx (/ h 4)))))
		(make-point (- x (* idx (/ offs 4))) y :absolute))))

(defun parallelogram-connect-point-R (cx cy w h dir offs idx)
  (if (eq dir :v)
	  (let ((x (+ cx (/ w 2)))
			(y (- cy (/ offs 2)))
			(h (- h (abs offs))))
		(make-point x (+ y (* idx (/ h 4))) :absolute))
	  (let ((x (+ cx (/ w 2) (- (/ (abs offs) 2))))
			(y (+ cy (* idx (/ h 4)))))
		(make-point (- x (* idx (/ offs 4))) y :absolute))))


(defun parallelogram-connect-point (center w h dir offs type1 type2 arg)
  (declare (ignore type1))
  (let ((cx (point-x center))
		(cy (point-y center))
		(handler (ecase type2
				   ((:center) #'parallelogram-connect-point-C)
				   ((:top)    #'parallelogram-connect-point-T)
				   ((:bottom) #'parallelogram-connect-point-B)
				   ((:left)   #'parallelogram-connect-point-L)
				   ((:right)  #'parallelogram-connect-point-R))))
	(funcall handler cx cy w h dir offs arg)))


;;-------------------------------------------------------------------------------
;;
;; class parallelogram
;;
;;-------------------------------------------------------------------------------
(defclass parallelogram (shape)
  ((center		:initform nil :initarg :center)		; point
   (width		:initform   0 :initarg :width)		; number
   (height		:initform   0 :initarg :height)		; number
   (direction	:initform   0 :initarg :direction)	; keyword :v :h
   (offset		:initform   0 :initarg :offset)		; number
   (fill		:initform nil :initarg :fill)		; (or nil fill-info)
   (stroke		:initform nil :initarg :stroke)		; (or nil fill-info)
   (filter		:initform nil :initarg :filter)))	; (or nil keyword)


(defmethod initialize-instance :after ((obj parallelogram) &rest initargs)
  (declare (ignore initargs))
  (with-slots (fill stroke filter layer) obj
	(setf fill   (make-fill   (or fill   *default-fill*   :none)))
	(setf stroke (make-stroke (or stroke *default-stroke* :none)))
	(setf filter (if (eq filter :none)
					 nil
					 (or filter *default-filter*)))
	(setf layer  (if (eq layer :none)
					 nil
					 (or layer *default-layer*))))
  obj)

(defmethod check ((obj parallelogram) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (center width height
					  direction offset fill stroke filter) obj
	(check-member width     :nullable nil :types number)
	(check-member height    :nullable nil :types number)
	(check-member direction :nullable nil :types keyword)
	(check-keywords direction :v :h)
	(check-member offset    :nullable nil :types number)
	(check-object fill      canvas dict :nullable nil :class   fill-info)
	(check-object stroke    canvas dict :nullable nil :class stroke-info)
	(check-member filter    :nullable   t :types keyword)
	(setf center (canvas-fix-point canvas center)))
  nil)

(defmethod attribute-width ((obj parallelogram))
  (slot-value obj 'width))

(defmethod attribute-height ((obj parallelogram))
  (slot-value obj 'height))

(defmethod attribute-center ((obj parallelogram))
  (slot-value obj 'center))

(defmethod shape-connect-point ((shp parallelogram) type1 type2 arg)
  (with-slots (center width height direction offset) shp
	(parallelogram-connect-point center width height
								 direction offset type1 type2 arg)))
  
;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((shp parallelogram)) ...)

(defmethod draw-entity ((obj parallelogram) writer)
  (with-slots (center width height
					  direction offset fill stroke filter) obj
	(labels ((make-points ()
			   (let ((tl (xy+ center (- (/ width 2)) (- (/ height 2))))
					 (tr (xy+ center (+ (/ width 2)) (- (/ height 2))))
					 (bl (xy+ center (- (/ width 2)) (+ (/ height 2))))
					 (br (xy+ center (+ (/ width 2)) (+ (/ height 2)))))
				 (if (eq direction :h)
					 (if (<= 0 offset)
						 `(,(x+ tl offset) ,bl ,(x+ br (- offset)) ,tr)
						 `(,tl ,(x+ bl (- offset)) ,br ,(x+ tr offset)))
					 (if (<= 0 offset)
						 `(,(y+ tl offset) ,bl ,(y+ br (- offset)) ,tr)
						 `(,tl ,(y+ bl offset) ,br ,(y+ tr (- offset)))))))
			 (format-points (pts)
			   (with-output-to-string (stream)
				 (do ((idx 0 (incf idx)))
					 ((null pts) nil)
				   (unless (zerop idx)
					 (princ #\space stream))
				   (format stream "~A,~A"
						   (coerce (point-x (car pts)) 'single-float)
						   (coerce (point-y (car pts)) 'single-float))
				   (setf pts (cdr pts))))))
	  (let ((id (and (not (entity-composition-p obj))
					 (slot-value obj 'id))))
		(pre-draw obj writer)
		(writer-write writer
					  "<polygon "
					  (write-when (keywordp id) "id='" id "' ")
					  (to-property-strings fill)
					  (to-property-strings stroke)
					  "points='" (format-points (make-points)) "' "
					  (write-when filter "filter='url(#" it ")' ")
					  "/>")
		(post-draw obj writer))))
  nil)
  

;;-------------------------------------------------------------------------------
;;
;; macro parallelogram
;;
;;-------------------------------------------------------------------------------
#|
#|EXPORT|#				:parallelogram
 |#
(defmacro parallelogram (center width height direction offset
						 &key fill stroke rotate link layer id filter contents)
  (let ((code `(register-entity (make-instance 'kaavio:parallelogram
											   :center ,center
											   :width ,width :height ,height
											   :direction ,direction :offset ,offset
											   :fill ,fill :stroke ,stroke
											   :rotate ,rotate :link ,link
											   :filter ,filter :layer ,layer :id ,id))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (kaavio:shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))

