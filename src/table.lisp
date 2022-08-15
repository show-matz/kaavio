#|
#|ASD|#				(:file "table"                     :depends-on ("cl-diagram"
#|ASD|#																"constants"
#|ASD|#																"group"
#|ASD|#																"font-info"
#|ASD|#																"fill-info"
#|ASD|#																"stroke-info"
#|ASD|#																"writer"))
#|EXPORT|#				;table.lisp
 |#


(in-package :cl-diagram)

(defun table-destructure-rc-keyword (kwd)
  (labels ((recur (lst acc)
			 (if (null lst)
				 (nreverse acc)
				 (let ((item (car lst)))
				   (cond
					 ((char= #\R item) (recur (cdr lst) (push :R acc)))
					 ((char= #\C item) (recur (cdr lst) (push :C acc)))
					 (t (let ((code (char-code item)))
						  (if (not (<= (char-code #\0) code (char-code #\9)))
							  (recur (cdr lst) (push item acc))
							  (progn
								(unless (integerp (car acc))
								  (push 0 acc))
								(setf (car acc) (+ (* (car acc) 10) (- code (char-code #\0))))
								(recur (cdr lst) acc))))))))))
	(handler-case
		(destructuring-bind (&key r c)
			(recur (coerce (symbol-name kwd) 'list) nil)
		  (values r c))
	  (t () (values nil nil)))))
						   

(labels ((recur (idx lst acc)
		   (if (null lst)
			   (values nil nil)
			   (if (zerop idx)
				   (let ((n (car lst)))
					 (values n (+ acc (/ n 2))))
				   (recur (1- idx) (cdr lst) (+ acc (car lst)))))))

  (defun table-get-cell-area (r c rows cols)
	(multiple-value-bind (w x) (recur c cols 0)
	  (multiple-value-bind (h y) (recur r rows 0)
		(values (list x y) w h))))

  (defun table-get-sub-area (kwd center rows cols)
	(let ((width  (apply #'+ cols))
		  (height (apply #'+ rows)))
	  (if (eq :rc kwd)
		  (values center width height)
		  (multiple-value-bind (r c) (table-destructure-rc-keyword kwd)
			(if (not (or r c))
				(values nil nil nil)
				(labels ((impl (idx lst)
						   (if idx
							   (recur idx lst 0)
							   (let ((total (apply #'+ lst)))
								 (values total (/ total 2))))))
				  (multiple-value-bind (w x) (impl c cols)
					(multiple-value-bind (h y) (impl r rows)
					  (if (not (and x y w h))
						  (values nil nil nil)
						  (values (list x y) w h)))))))))))

(defun table-normalize-texts (texts default-font)
  (labels ((fix-data (data)
			 (if (null data)
				 nil	;; nil means 'no text cell'
				 (destructuring-bind (d &key font align valign)
									 (if (listp data) data (cons data nil))
				   ;; auto aligning when no align specified.
				   (unless align
					 (setf align (typecase d
								   (number  :right)
								   (symbol  :center)
								   (keyword :center)
								   (t       :left))))
				   `(,(if (stringp d)
						  d
						  (format nil "~A" d))
					  ,align ,(or valign :center) ,(or font default-font)))))
		   (row-impl (lst acc)
			 (if (null lst)
				 (nreverse acc)
				 (row-impl (cdr lst)
						   (push (fix-data (car lst)) acc))))
		   (tbl-impl (lst acc)
			 (if (null lst)
				 (nreverse acc)
				 (tbl-impl (cdr lst)
						   (push (row-impl (car lst) nil) acc)))))
	(tbl-impl texts nil)))
		

(defun table-fix-text (pt w h lst)
  (destructuring-bind (txt align valign font) lst
	(let* ((fs/2 (/ (slot-value font 'size) 2))
		   (pivot (case align
					((:center) (y+  pt fs/2))
					((:right)  (xy+ pt (- (/ w 2) fs/2) fs/2))
					((:left)   (xy+ pt (- fs/2 (/ w 2)) fs/2)))))
	  (case valign
		((:bottom) (setf (point-y pivot) (+ (point-y pt)    (/ h 2)  (- fs/2))))
		((:top)    (setf (point-y pivot) (+ (point-y pt) (- (/ h 2)) (*  3 fs/2)))))
	  (values pivot txt align font))))


#|
#|EXPORT|#				:*default-table-font*
#|EXPORT|#				:*default-table-stroke*
#|EXPORT|#				:*default-table-fill*
#|EXPORT|#				:*default-table-layer*
 |#
(defparameter *default-table-font*   nil)
(defparameter *default-table-stroke* nil)
(defparameter *default-table-fill*   nil)
(defparameter *default-table-layer*  nil)

;;-------------------------------------------------------------------------------
;;
;; class table
;;
;;-------------------------------------------------------------------------------
(defclass table (group)
  ((rows		:initform nil :initarg :rows)		; list of integers
   (cols		:initform nil :initarg :cols)		; list of integers
   (font		:initform nil :initarg :font)		; (or nil font-info)
   (stroke		:initform nil :initarg :stroke)		; (or nil stroke-info)
   (fills		:initform nil :initarg :fills)		; list
   (texts		:initform nil :initarg :texts)))	; list


(defmethod initialize-instance :after ((tbl table) &rest initargs)
  (declare (ignore initargs))
  (labels ((fix-fills (lst acc)
			 (if (null lst)
				 (nreverse acc)
				 (progn
				   (push (car lst) acc)
				   (push (make-fill (cadr lst)) acc)
				   (fix-fills (cddr lst) acc)))))
	(with-slots (font stroke fills layer) tbl
	  (setf font   (make-font   (or font   *default-table-font*   *default-font*)))
	  (setf stroke (make-stroke (or stroke *default-table-stroke* *default-stroke*)))
	  (when (and (null fills) *default-table-fill*)
		(setf fills (list :rc *default-table-fill*)))
	  (when (and (null fills) *default-fill*)
		(setf fills (list :rc *default-fill*)))
	  (setf fills  (fix-fills fills nil))
	  (setf layer  (if (eq layer :none)
					   nil
					   (or layer *default-table-layer* *default-layer*)))))
  tbl)

(defmethod check ((tbl table) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (font fills stroke texts) tbl
	(labels ((chk-fills (lst)
			   (when lst
				 (let ((fill (cadr fills)))
				   (check-object fill canvas dict :nullable nil :class fill-info)))))
	  (chk-fills fills))
	(check-object font      canvas dict :nullable t :class font-info)
	(check-object stroke    canvas dict :nullable t :class stroke-info)
	(setf texts (table-normalize-texts texts font)))
  nil)

;; override of group::draw-group
(defmethod draw-group ((tbl table) writer)
  (let ((canvas (group-get-canvas tbl)))
	(let ((width  (canvas-width  canvas))
		  (height (canvas-height canvas)))
	  (macrolet ((register-entity (entity)
				   `(check-and-draw-local-entity ,entity canvas writer)))
		(with-slots (center rows cols stroke fills font texts) tbl
		  ;; filling ----------------------------------------------
		  (labels ((fill-impl (lst)
					 (when lst
					   (let ((kwd  (car  lst))
							 (info (cadr lst)))
						 (multiple-value-bind (c w h)
									 (table-get-sub-area kwd center rows cols)
						   (when (and c w h)
							 (rect c w h :fill info :stroke :none)))
						 (fill-impl (cddr lst))))))
			(fill-impl fills))
		  ;; draw lines -------------------------------------------
		  (let ((y 0))
			(line `((0 ,y) (,width ,y)) :stroke stroke)
			(dolist (r rows)
			  (incf y r)
			  (line `((0 ,y) (,width ,y)) :stroke stroke)))
		  (let ((x 0))
			(line `((,x ,0) (,x ,height)) :stroke stroke)
			(dolist (c cols)
			  (incf x c)
			  (line `((,x ,0) (,x ,height)) :stroke stroke)))
		  ;; draw texts -------------------------------------------
		  (let ((r 0))
			(dolist (row texts)
			  (let ((c 0))
				(dolist (data row)
				  (when data
					(multiple-value-bind (pt w h) (table-get-cell-area r c rows cols)
					  (multiple-value-bind (pt txt align fnt) (table-fix-text pt w h data)
						(text pt txt :align align :font (or fnt font)))))
				  (incf c)))
			  (incf r)))))))
  nil)
  

;;-------------------------------------------------------------------------------
;;
;; macro table
;;
;;-------------------------------------------------------------------------------
#|
#|EXPORT|#				:table
 |#
(defmacro table (center rows cols &key font fills stroke texts layer id)
  `(register-entity (make-instance 'diagram:table
								   :center ,center
								   :width  (apply #'+ ,cols)
								   :height (apply #'+ ,rows)
								   :rows ,rows :cols ,cols
								   :stroke ,stroke :fills ,fills
								   :font ,font :texts ,texts :layer ,layer :id ,id)))



;;------------------------------------------------------------------------------
;;
;; macro with-table-options
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:with-table-options
 |#
(defmacro with-table-options ((&key font fill stroke layer) &rest body)
  (labels ((impl (params acc)
			 (if (null params)
				 acc
				 (let ((value  (car  params))
					   (symbol (cadr params)))
				   (impl (cddr params)
						 (if (null value)
							 acc
							 (push (list symbol value) acc)))))))
	(let ((lst (impl (list font   '*default-table-font*
						   fill   '*default-table-fill*
						   stroke '*default-table-stroke*
						   layer  '*default-table-layer*) nil)))
	  `(let ,lst
		 ,@body))))

;;-------------------------------------------------------------------------------
;;
;; macro with-table-cell
;;
;;-------------------------------------------------------------------------------
#|
#|EXPORT|#				:with-table-cell
 |#
(defmacro with-table-cell ((id r c) &body body)
  (let ((g-tbl     (gensym "TBL"))
		(g-center  (gensym "CENTER"))
		(g-width   (gensym "WIDTH"))
		(g-height  (gensym "HEIGHT"))
		(g-topleft (gensym "TOPLEFT")))
	`(let ((,g-tbl (diagram::dict-get-entity (diagram::get-dictionary) ,id)))
	   (multiple-value-bind (,g-center ,g-width ,g-height)
				(diagram::table-get-cell-area ,r ,c (slot-value ,g-tbl 'diagram::rows)
													(slot-value ,g-tbl 'diagram::cols))
		 (let* ((,g-topleft (point+ (shape-topleft ,g-tbl)
								   (xy+ ,g-center (- (/ ,g-width 2)) (- (/ ,g-height 2)))))
				(canvas (make-canvas ,g-topleft ,g-width ,g-height)))
		   (declare (special canvas))
		   ,@body)))))

