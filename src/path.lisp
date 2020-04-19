#|
#|ASD|#				(:file "path"                      :depends-on ("cl-diagram"
#|ASD|#																"constants"
#|ASD|#																"fill-info"
#|ASD|#																"stroke-info"
#|ASD|#																"entity"
#|ASD|#																"writer"))
#|EXPORT|#				;path.lisp
 |#

(in-package :cl-diagram)

;-------------------------------------------------------------------------------
;
; helper macros
;
;-------------------------------------------------------------------------------
#|
#|EXPORT|#				:move-to
#|EXPORT|#				:close-path
#|EXPORT|#				:line-to
#|EXPORT|#				:h-line-to
#|EXPORT|#				:v-line-to
#|EXPORT|#				:arc-to
#|EXPORT|#				:2d-curve-to
#|EXPORT|#				:3d-curve-to
 |#
(defun move-to (x y) `(:move-to ,x ,y))
(defun close-path ()  :close-path)
(defun line-to (x y &rest more)  `(:line-to ,x ,y ,@more))
(defun h-line-to (x)  `(:h-line-to ,x))
(defun v-line-to (y)  `(:v-line-to ,y))
(defun arc-to (rx ry x-axis-rotation large-arc-flag sweep-flag x y)
  `(:arc-to ,rx ,ry ,x-axis-rotation ,large-arc-flag ,sweep-flag ,x ,y))

(defun 2d-curve-x2 (x y)				`(:t-curve-to ,x ,y))
(defun 2d-curve-x4 (x1 y1 x y)			`(:q-curve-to ,x1 ,y1 ,x ,y))
(defun 3d-curve-x4 (x2 y2 x y)			`(:s-curve-to ,x2 ,y2 ,x ,y))
(defun 3d-curve-x6 (x1 y1 x2 y2 x y)	`(:c-curve-to ,x1 ,y1 ,x2 ,y2 ,x ,y))

(defmacro 2d-curve-to (&rest params)
  (ecase (length params)
	((2) `(2d-curve-x2 ,@params))
	((4) `(2d-curve-x4 ,@params))))
	
(defmacro 3d-curve-to (&rest params)
  (ecase (length params)
	((4) `(3d-curve-x4 ,@params))
	((6) `(3d-curve-x6 ,@params))))


;-------------------------------------------------------------------------------
;
; internal helper functions
;
;-------------------------------------------------------------------------------
(macrolet ((push-cmd (v)
			 `(push (if (eq mode :absolute)
						,(string-upcase v) ,v) acc))
		   (chk-loop (cnt kwd &rest fncs)
			 (type-assert cnt integer)
			 (type-assert kwd keyword)
			 (labels ((conds (n acc)
						(if (zerop n)
							acc
							(let ((sym (intern (string-upcase (format nil "~:r" n)) :common-lisp)))
							  (conds (1- n) (push `(numberp (,sym lst)) acc)))))
					  (push-num (cnt tmp exprs)
						(if (zerop cnt)
							(nreverse tmp)
							(progn
							  (push `(push (let ((it (car lst)))
											 (if (eq mode :relative)
												 it
												 ,(car exprs))) acc) tmp)
							  (push '(setf lst (cdr lst)) tmp)
							  (push-num (1- cnt) tmp (cdr exprs))))))
			   `(do ((cnt 0 (1+ cnt)))
					(nil)
				  (unless (and (<= ,cnt (length lst))
							   ,@(conds cnt nil))
					(if (< 0 cnt)
						(return)
						(throw-exception ,(format nil "~A needs at least ~A parameters." kwd cnt))))
				  ,@(push-num cnt nil fncs)))))

  (defun __chk-&-fix-absolute (lst acc mode canvas)
	(declare (ignore mode canvas))
	(values lst acc :absolute))

  (defun __chk-&-fix-relative (lst acc mode canvas)
	(declare (ignore mode canvas))
	(values lst acc :relative))

  (defun __chk-&-fix-close-path (lst acc mode canvas)
	(declare (ignore canvas))
	(push-cmd "z")
	(values lst acc mode))

  (defun __chk-&-fix-move-to (lst acc mode canvas)
	(push-cmd "m")
	(let ((x (canvas-left canvas))
		  (y (canvas-top canvas)))
	  (chk-loop 2 :move-to (+ it x) (+ it y)))
    (values lst acc mode))

  (defun __chk-&-fix-line-to (lst acc mode canvas)
	(push-cmd "l")
	(let ((x (canvas-left canvas))
		  (y (canvas-top canvas)))
	  (chk-loop 2 :line-to (+ it x) (+ it y)))
    (values lst acc mode))

  (defun __chk-&-fix-h-line-to (lst acc mode canvas)
	(push-cmd "h")
	(let ((y (canvas-top canvas)))
	  (chk-loop 1 :h-line-to (+ it y)))
	(values lst acc mode))

  (defun __chk-&-fix-v-line-to (lst acc mode canvas)
	(push-cmd "v")
	(let ((x (canvas-left canvas)))
	  (chk-loop 1 :v-line-to (+ it x)))
	(values lst acc mode))

  (defun __chk-&-fix-arc-to (lst acc mode canvas)
	(push-cmd "a")
	(let ((x (canvas-left canvas))
		  (y (canvas-top canvas)))
	  (chk-loop 7 :arc-to it it it it it (+ it x) (+ it y)))
	(values lst acc mode))

  (defun __chk-&-fix-t-curve-to (lst acc mode canvas)
	(push-cmd "t")
	(let ((x (canvas-left canvas))
		  (y (canvas-top canvas)))
	  (chk-loop 2 :t-curve-to (+ it x) (+ it y)))
	(values lst acc mode))

  (defun __chk-&-fix-q-curve-to (lst acc mode canvas)
	(push-cmd "q")
	(let ((x (canvas-left canvas))
		  (y (canvas-top canvas)))
	  (chk-loop 4 :q-curve-to (+ it x) (+ it y) (+ it x) (+ it y)))
	(values lst acc mode))

  (defun __chk-&-fix-s-curve-to (lst acc mode canvas)
	(push-cmd "s")
	(let ((x (canvas-left canvas))
		  (y (canvas-top canvas)))
	  (chk-loop 4 :s-curve-to (+ it x) (+ it y) (+ it x) (+ it y)))
	(values lst acc mode))

  (defun __chk-&-fix-c-curve-to (lst acc mode canvas)
	(push-cmd "c")
	(let ((x (canvas-left canvas))
		  (y (canvas-top canvas)))
	  (chk-loop 6 :c-curve-to (+ it x) (+ it y)
							  (+ it x) (+ it y) (+ it x) (+ it y)))
	(values lst acc mode)))


(defun check-and-fix-d-data (lst acc mode canvas)
  (if (null lst)
	  (nreverse acc)
	  (let ((fnc (case (car lst)
				   ((:absolute)   #'__chk-&-fix-absolute)
				   ((:relative)   #'__chk-&-fix-relative)
				   ((:move-to)    #'__chk-&-fix-move-to)
				   ((:close-path) #'__chk-&-fix-close-path)
				   ((:line-to)    #'__chk-&-fix-line-to)
				   ((:h-line-to)  #'__chk-&-fix-h-line-to)
				   ((:v-line-to)  #'__chk-&-fix-v-line-to)
				   ((:arc-to)     #'__chk-&-fix-arc-to)
				   ((:t-curve-to) #'__chk-&-fix-t-curve-to)
				   ((:q-curve-to) #'__chk-&-fix-q-curve-to)
				   ((:s-curve-to) #'__chk-&-fix-s-curve-to)
				   ((:c-curve-to) #'__chk-&-fix-c-curve-to)
				   (t (throw-exception "Invalid d sequence for path.")))))
		(multiple-value-setq (lst acc mode)
		  (funcall fnc (cdr lst) acc mode canvas))
		(check-and-fix-d-data lst acc mode canvas))))


;-------------------------------------------------------------------------------
;
; class path
;
;-------------------------------------------------------------------------------
(defclass path (entity)
  ((data	:initform nil :initarg :data)		; list
   (class	:initform nil :initarg :class)		; keyword
   (fill	:initform nil :initarg :fill)		; (or nil fill-info)
   (stroke	:initform nil :initarg :stroke)))	; (or nil stroke-info)


(defmethod initialize-instance :after ((ent path) &rest initargs)
  (declare (ignore initargs))
  (with-slots (fill stroke) ent
	(setf fill   (make-fill   (or fill   *default-fill*)))
	(setf stroke (make-stroke (or stroke *default-stroke*))))
  ent)
  
(defmethod check ((ent path) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (data class fill stroke) ent
	(check-member data    :nullable nil :types list)
	(check-member class   :nullable   t :types (or keyword string))
	(check-object fill    canvas dict :nullable nil :class fill-info)
	(check-object stroke  canvas dict :nullable nil :class stroke-info)
	(let ((lst (onlisp/flatten data)))
	  (dolist (elm lst)
		(chk-type elm (or keyword number))
		(when (keywordp elm)
		  (check-keywords elm
						  :absolute :relative :move-to :close-path
						  :line-to :h-line-to :v-line-to :arc-to
						  :t-curve-to :q-curve-to :s-curve-to :c-curve-to)))
	  (setf data (check-and-fix-d-data lst nil :absolute canvas))))
  nil)
	
(defmethod draw-entity ((ent path) writer)
  (labels ((format-path-data (d-list)
			 (with-output-to-string (stream)
			   (let ((idx 0))
				 (dolist (elm d-list)
				   (unless (zerop idx)
					 (princ #\space stream))
				   (cond
					 ((integerp elm) (format stream "~A" elm))
					 ((numberp  elm) (format stream "~F" elm))
					 ((stringp  elm) (format stream "~A" elm)))
				   (incf idx))))))
	(with-slots (data class fill stroke) ent
	  (let ((id  (and (not (entity-composition-p ent))
					  (slot-value ent 'id))))
		(pre-draw ent writer)
		(writer-write writer
					  "<path "
					  (write-when id       "id='" it "' ")
					  (write-when class "class='" it "' ")
					  (unless class
						(when fill
						  (to-property-strings fill)))
					  (unless class
						(when stroke
						  (to-property-strings stroke)))
					  "d='" (format-path-data data) "' "
					  "/>")
		(pre-draw ent writer)))))


#|
#|EXPORT|#				:path
 |#
(defmacro path (data &key class fill stroke layer id)
  `(register-entity (make-instance 'diagram:path
								   :data ,data :class ,class :fill ,fill
								   :stroke ,stroke :layer ,layer :id ,id)))

