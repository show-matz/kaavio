#|
#|ASD|#				(:file "path"                      :depends-on ("kaavio"
#|ASD|#																"constants"
#|ASD|#																"fill-info"
#|ASD|#																"stroke-info"
#|ASD|#																"entity"
#|ASD|#																"filter"
#|ASD|#																"writer"))
#|EXPORT|#				;path.lisp
 |#

(in-package :kaavio)

;;------------------------------------------------------------------------------
;;
;; internal helper functions
;;
;;------------------------------------------------------------------------------
(macrolet ((push-cmd (v)
			 `(push (if (eq mode :absolute)
						(string-upcase ,v) ,v) acc))
		   (push-point (pt)
			 `(progn
				(push (kaavio:point-x ,pt) acc)
				(push (kaavio:point-y ,pt) acc))))

  (defun __check-&-fix-move-to (param acc mode canvas)
	(unless param
	  (throw-exception "No param for :move-to in path."))
	(let ((cmd "m"))
	  (dolist (pt param)
		(unless (kaavio:point-p pt)
		  (throw-exception "Invalid param for :move-to in path."))
		(cond
		  ((kaavio:point-absolute-p pt)
		   (let ((mode :absolute))
			 (push-cmd cmd)
			 (push-point pt)))
		  ((eq mode :relative)
		   (progn
			 (push-cmd cmd)
			 (push-point pt)))
		  ((eq mode :absolute)
		   (progn
			 (push-cmd   cmd)
			 (push-point (kaavio:point+ pt (kaavio:canvas-topleft canvas))))))
		(setf cmd "l")))
	(values mode acc))

  (defun __check-&-fix-line-to (param acc mode canvas)
	(unless param
	  (throw-exception "No param for :line-to in path."))
	(let ((cmd "l"))
	  (dolist (pt param)
		(unless (kaavio:point-p pt)
		  (throw-exception "Invalid param for :line-to in path."))
		(cond
		  ((kaavio:point-absolute-p pt)
		   (let ((mode :absolute))
			 (push-cmd cmd)
			 (push-point pt)))
		  ((eq mode :relative)
		   (progn
			 (push-cmd cmd)
			 (push-point pt)))
		  ((eq mode :absolute)
		   (progn
			 (push-cmd   cmd)
			 (push-point (kaavio:point+ pt (kaavio:canvas-topleft canvas))))))))
	(values mode acc))

  (defun __check-&-fix-h-line-to (param acc mode canvas)
	(unless (= 1 (length param))
	  (throw-exception "No param for :h-line-to in path."))
	(let ((cmd "h")
		  (param (car param)))
	  (if (and (kaavio:point-p param) (kaavio:point-absolute-p param))
		  (let ((mode :absolute))
			(push-cmd cmd)
			(push (kaavio:point-x param) acc))
		  (progn
			(when (kaavio:point-p param)
			  (setf param (kaavio:point-x param)))
			(if (eq mode :relative)
				(progn
				  (push-cmd cmd)
				  (push param acc))
				(progn
				  (push-cmd cmd)
				  (push (+ param (kaavio:canvas-left canvas)) acc))))))
	(values mode acc))

  (defun __check-&-fix-v-line-to (param acc mode canvas)
	(unless (= 1 (length param))
	  (throw-exception "No param for :v-line-to in path."))
	(let ((cmd "v")
		  (param (car param)))
	  (if (and (kaavio:point-p param) (kaavio:point-absolute-p param))
		  (let ((mode :absolute))
			(push-cmd cmd)
			(push (kaavio:point-y param) acc))
		  (progn
			(when (kaavio:point-p param)
			  (setf param (kaavio:point-y param)))
			(if (eq mode :relative)
				(progn
				  (push-cmd cmd)
				  (push param acc))
				(progn
				  (push-cmd cmd)
				  (push (+ param (kaavio:canvas-top canvas)) acc))))))
	(values mode acc))

  (defun __check-&-fix-arc-to (param acc mode canvas)
	(unless (= 6 (length param))
	  (throw-exception "Invalid param for :arc-to in path."))
	(destructuring-bind (rx ry x-axis-rotation
							large-arc-flag sweep-flag pt) param
	  ;;ToDo : check each params...
	  (unless (kaavio:point-p pt)
		(throw-exception "Invalid pt param for :arc-to in path."))
	  (let ((mode (if (kaavio:point-absolute-p pt) :absolute mode)))
		(push-cmd				"a")
		(push rx				acc)
		(push ry				acc)
		(push x-axis-rotation	acc)
		(push large-arc-flag	acc)
		(push sweep-flag		acc)
		(cond
		  ((kaavio:point-absolute-p pt)	(push-point pt))
		  ((eq mode :relative)				(push-point pt))
		  ((eq mode :absolute) (push-point (kaavio:point+ pt (kaavio:canvas-topleft canvas)))))))
	(values mode acc))

  (defun __check-&-fix-2d-curve-to (param acc mode canvas)
	;;ToDo : 2 points の場合で、片方だけ absolute-point だった場合はどうするのか？
	;;ToDo : 現状では、point の absolute-ness は考慮しない実装になっている。
	(let ((len (length param)))
	  (unless (and (<= 1 len) (<= len 2))
		(throw-exception "Invalid param for :2d-curve-to in path."))
	  (let ((cmd (if (= len 1) "t" "q")))
		(push-cmd cmd)
		(dolist (pt param)
		  (unless (kaavio:point-p pt)
			(throw-exception "Invalid point param for :2d-curve-to in path."))
		  (cond
			((kaavio:point-absolute-p pt) (push-point pt))
			((eq mode :relative) (push-point pt))
			((eq mode :absolute) (push-point (kaavio:point+ pt (kaavio:canvas-topleft canvas))))))))
	(values mode acc))

  (defun __check-&-fix-3d-curve-to (param acc mode canvas)
	;;ToDo : 2 points 以上の場合で、一部だけ absolute-point だった場合はどうするのか？
	;;ToDo : 現状では、point の absolute-ness は考慮しない実装になっている。
	(let ((len (length param)))
	  (unless (and (<= 2 len) (<= len 3))
		(throw-exception "Invalid param for :3d-curve-to in path."))
	  (let ((cmd (if (= len 2) "s" "c")))
		(push-cmd cmd)
		(dolist (pt param)
		  (unless (kaavio:point-p pt)
			(throw-exception "Invalid point param for :2d-curve-to in path."))
		  (cond
			((kaavio:point-absolute-p pt) (push-point pt))
			((eq mode :relative) (push-point pt))
			((eq mode :absolute) (push-point (kaavio:point+ pt (kaavio:canvas-topleft canvas))))))))
	(values mode acc)))

(defun __check-&-fix-data (lst acc mode canvas)
  (if (null lst)
	  (nreverse acc)
	  (let ((next (car lst))
			(rest (cdr lst)))
		(cond
		  ((keywordp next)
		   (case next
			 ((:absolute)	(setf mode :absolute))
			 ((:relative)	(setf mode :relative))
			 ((:close-path)	(push (if (eq mode :absolute) "Z" "z") acc))))
		  ((listp next)
		   (let ((fnc (case (car next)
						((:move-to)		#'__check-&-fix-move-to)
						((:line-to)		#'__check-&-fix-line-to)
						((:h-line-to)	#'__check-&-fix-h-line-to)
						((:v-line-to)	#'__check-&-fix-v-line-to)
						((:arc-to)		#'__check-&-fix-arc-to)
						((:2d-curve-to)	#'__check-&-fix-2d-curve-to)
						((:3d-curve-to)	#'__check-&-fix-3d-curve-to)
						(t (throw-exception "Invalid d sequence for path.")))))
			 (multiple-value-setq (mode acc)
					   (funcall fnc (cdr next) acc mode canvas))))
		  (t (throw-exception "Invalid d sequence for path.")))
		(__check-&-fix-data rest acc mode canvas))))


;;------------------------------------------------------------------------------
;;
;; class path
;;
;;------------------------------------------------------------------------------
(defclass path (entity)
  ((data	:initform nil :initarg :data)		; list
   (fill	:initform nil :initarg :fill)		; (or nil fill-info)
   (stroke	:initform nil :initarg :stroke)		; (or nil stroke-info)
   (filter	:initform nil :initarg :filter)))	; (or nil keyword)


(defmethod initialize-instance :after ((ent path) &rest initargs)
  (declare (ignore initargs))
  (with-slots (fill stroke filter layer) ent
	(setf fill   (make-fill   (or fill   *default-fill*   :none)))
	(setf stroke (make-stroke (or stroke *default-stroke* :none)))
	(setf filter (if (eq filter :none)
					 nil
					 (or filter *default-filter*)))
	(setf layer  (if (eq layer :none)
					 nil
					 (or layer *default-layer*))))
  ent)
  
(defmethod check ((ent path) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (data fill stroke filter) ent
	(check-member data    :nullable nil :types list)
	(check-object fill    canvas dict :nullable nil :class fill-info)
	(check-object stroke  canvas dict :nullable nil :class stroke-info)
	(check-member filter  :nullable   t :types keyword)
	(setf data (__check-&-fix-data data nil :absolute canvas)))
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
	(with-slots (data fill stroke filter) ent
	  (let ((id  (and (not (entity-composition-p ent))
					  (slot-value ent 'id))))
		(pre-draw ent writer)
		(writer-write writer
					  "<path "
					  (write-when (keywordp id) "id='" id "' ")
					  (to-property-strings fill)
					  (to-property-strings stroke)
					  "d='" (format-path-data data) "' "
					  (write-when filter "filter='url(#" it ")' ")
					  "/>")
		(post-draw ent writer)))))


;;------------------------------------------------------------------------------
;;
;; macro path
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:path
 |#
(defmacro path (data &key fill stroke layer filter id)
  `(register-entity (make-instance 'kaavio:path
								   :data ,data :fill ,fill :stroke ,stroke
								   :layer ,layer :filter ,filter :id ,id)))

