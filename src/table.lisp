#|
#|ASD|#				(:file "table"                     :depends-on ("kaavio"
#|ASD|#																"constants"
#|ASD|#																"group"
#|ASD|#																"font-info"
#|ASD|#																"fill-info"
#|ASD|#																"stroke-info"
#|ASD|#																"writer"))
#|EXPORT|#				;table.lisp
 |#


(in-package :kaavio)

;; :rN => (values N N nil nil)
;; :cN => (values nil nil N N)
;; :rNcM => (values N N M M)
;; :rN-M => (values N M nil nil)
;; :cN-M => (values nil nil N M)
;; :rJ-KcN-M => (values J K N M)
;; ToDo : :r0c1r2 みたいのがエラーにならない
(defun table-destructure-rc-keyword (kwd)
  (labels ((recur (mode lst acc)
			 (if (null lst)
				 (nreverse acc)
				 (let ((item (car lst)))
				   (cond
					 ((char= #\R item) (recur :R (cdr lst) (push :R1 acc)))
					 ((char= #\C item) (recur :C (cdr lst) (push :C1 acc)))
					 ((char= #\- item) (cond
										 ((eq mode :R) (recur :R  (cdr lst) (push :R2 acc)))
										 ((eq mode :C) (recur :C  (cdr lst) (push :C2 acc)))
										 (t            (recur nil (cdr lst) acc))))
					 (t (let ((code (char-code item)))
						  (if (not (<= (char-code #\0) code (char-code #\9)))
							  (recur mode (cdr lst) (push item acc))
							  (progn
								(unless (integerp (car acc))
								  (push 0 acc))
								(setf (car acc) (+ (* (car acc) 10) (- code (char-code #\0))))
								(recur mode (cdr lst) acc))))))))))
	(handler-case
		(destructuring-bind (&key r1 r2 c1 c2)
			(recur nil (coerce (symbol-name kwd) 'list) nil)
		  (values r1 (or r2 r1)
				  c1 (or c2 c1)))
	  (t () (values nil nil nil nil)))))
						   

(labels ((recur (idx lst acc)
		   (if (null lst)
			   (values nil nil)
			   (let ((n (car lst)))
				 (if (zerop idx)
					 (values n (+ acc (/ n 2)))
					 (recur (1- idx) (cdr lst) (+ acc n))))))
		 (recur-range (idx1 idx2 lst pre acc)
		   (if (zerop idx2)
			   (values acc (+ pre (/ acc 2)))
			   (if (null lst)
				   (values nil nil)
				   (let ((n (car lst)))
					 (recur-range (1- idx1) (1- idx2) (cdr lst)
								  (+ pre (if (< 0 idx1) n 0))
								  (+ acc (if (and (<= idx1 0) (< 0 idx2)) n 0))))))))

  (defun table-get-cell-area (r c rows cols)
	(multiple-value-bind (w x) (recur c cols 0)
	  (multiple-value-bind (h y) (recur r rows 0)
		(values (list x y) w h))))

  ;; returns (values center-pt width height)
  (defun table-get-sub-area (kwd center rows cols)
	(if (eq :rc kwd)
		(values center (apply #'+ cols) (apply #'+ rows))
		(multiple-value-bind (r1 r2 c1 c2) (table-destructure-rc-keyword kwd)
		  (if (not (or r1 c1))
			  (values nil nil nil)
			  (labels ((impl (idx1 idx2 lst)
						 (if (not (and idx1 idx2))
							 (let ((total (apply #'+ lst)))
							   (values total (/ total 2)))
							 (cond
							   ((= idx1 idx2) (recur idx1 lst 0))
							   ((< idx1 idx2) (recur-range idx1 (1+ idx2) lst 0 0))
							   ((< idx2 idx1) (recur-range idx2 (1+ idx1) lst 0 0))))))
				(multiple-value-bind (w x) (impl c1 c2 cols)
				  (multiple-value-bind (h y) (impl r1 r2 rows)
					(if (not (and x y w h))
						(values nil nil nil)
						(values (list x y) w h))))))))))

(defun table-normalize-texts (texts)
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
					  ,align ,(or valign :center) ,font))))
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
		

(defun table-fix-text (pt w h lst default-font)
  (destructuring-bind (txt align valign font) lst
	(let* ((fnt  (or font default-font))
		   (fs/2 (/ (slot-value fnt 'size) 2))
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
				   (check-object fill canvas dict :nullable nil :class fill-info))
				 (chk-fills (cdr lst)))))
	  (chk-fills fills))
	(check-object font      canvas dict :nullable t :class font-info)
	(check-object stroke    canvas dict :nullable t :class stroke-info)
	(setf texts (table-normalize-texts texts)))
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
		  ;; 「同じ色指定が連続するなら g tag でまとめたい」が、keyword とは限らないので断念
		  (unless (and (= 2 (length fills))
					   (eq :none (slot-value (cadr fills) 'kaavio::color)))
			(writer-write writer "<g stroke='none'>")
			(writer-incr-level writer)
			(let ((*mute-stroke* t))
			  (labels ((fill-impl (lst)
						 (when lst
						   (let ((kwd  (car  lst))
								 (info (cadr lst)))
							 (multiple-value-bind (c w h)
								 (table-get-sub-area kwd center rows cols)
							   (when (and c w h)
								 (rect c w h :fill info :stroke :none)))
							 (fill-impl (cddr lst))))))
				(fill-impl fills)))
			(writer-decr-level writer)
			(writer-write writer "</g>"))
		  ;; draw lines -------------------------------------------
		  ;; ToDo : lines の custom drawing をサポートしたい（これは壮大）
		  (unless (eq :none (slot-value stroke 'color))
			(writer-write writer "<g " (to-property-strings stroke) " fill='none'>")
			(writer-incr-level writer)
			(let ((*mute-fill*   t)
				  (*mute-stroke* t))
			  (rect center (reduce #'+ cols)  (reduce #'+ rows) :fill :none :stroke stroke)
			  (let ((y 0))
				(dolist (r (butlast rows))
				  (incf y r)
				  (line `((0 ,y) (,width ,y)) :stroke stroke)))
			  (let ((x 0))
				(dolist (c (butlast cols))
				  (incf x c)
				  (line `((,x ,0) (,x ,height)) :stroke stroke))))
			(writer-decr-level writer)
			(writer-write writer "</g>"))
		  ;; draw texts -------------------------------------------
		  (when texts
			(writer-write writer "<g " (to-property-strings font) " >")
			(writer-incr-level writer)
			(let ((r 0))
			  (dolist (row texts)
				(let ((c 0))
				  (dolist (data row)
					(when data
					  (multiple-value-bind (pt w h) (table-get-cell-area r c rows cols)
						(multiple-value-bind (pt txt align fnt) (table-fix-text pt w h data font)
						  (let ((*mute-font* (null fnt)))
							(text pt txt :align align :font (or fnt font))))))
					(incf c)))
				(incf r)))
			(writer-decr-level writer)
			(writer-write writer "</g>"))))))
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
  `(register-entity (make-instance 'kaavio:table
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
	`(let ((,g-tbl (kaavio::dict-get-entity (kaavio::get-dictionary) ,id)))
	   (multiple-value-bind (,g-center ,g-width ,g-height)
				(kaavio::table-get-cell-area ,r ,c (slot-value ,g-tbl 'kaavio::rows)
													(slot-value ,g-tbl 'kaavio::cols))
		 (let* ((,g-topleft (point+ (attribute-topleft ,g-tbl)
								   (xy+ ,g-center (- (/ ,g-width 2)) (- (/ ,g-height 2)))))
				(canvas (make-canvas ,g-topleft ,g-width ,g-height)))
		   (declare (special canvas))
		   ,@body)))))

#|
#|EXPORT|#				:with-table-range
 |#
(defmacro with-table-range ((id kwd) &body body)
  (let ((g-tbl     (gensym "TBL"))
		(g-center  (gensym "CENTER"))
		(g-width   (gensym "WIDTH"))
		(g-height  (gensym "HEIGHT"))
		(g-topleft (gensym "TOPLEFT")))
	`(let ((,g-tbl (kaavio::dict-get-entity (kaavio::get-dictionary) ,id)))
	   (multiple-value-bind (,g-center ,g-width ,g-height)
				(kaavio::table-get-sub-area ,kwd (slot-value ,g-tbl 'kaavio::center)
												 (slot-value ,g-tbl 'kaavio::rows)
												 (slot-value ,g-tbl 'kaavio::cols))
		 (let* ((,g-topleft (point+ (attribute-topleft ,g-tbl)
									(xy+ ,g-center (- (/ ,g-width 2)) (- (/ ,g-height 2)))))
				(canvas (make-canvas ,g-topleft ,g-width ,g-height)))
		   (declare (special canvas))
		   ,@body)))))

