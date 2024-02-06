#|
#|ASD|#				(:file "brace"                     :depends-on ("kaavio"
#|ASD|#																"constants"
#|ASD|#																"path"))
#|EXPORT|#				;brace.lisp
 |#

(in-package :kaavio)

#|
#|EXPORT|#				:*default-brace-font*
#|EXPORT|#				:*default-brace-stroke*
#|EXPORT|#				:*default-brace-filter*
#|EXPORT|#				:*default-brace-layer*
 |#
(defparameter *default-brace-font*         nil)
(defparameter *default-brace-stroke*       nil)
(defparameter *default-brace-filter*       nil)
(defparameter *default-brace-layer*        nil)

(defun brace-make-path-left (w h r point)
  (let ((r (or r (/ w 3)))
		(point (or point (/ h 2))))
	(let ((rx (/ w 2))
		  (ry (min r (/ point 2) (/ (- h point) 2))))
	  (values point
			  `((:move-to (0 0))
				(:arc-to ,rx ,ry 1 0 1 (,rx ,ry))
				(:line-to (,rx ,(- point ry)))
				(:arc-to ,rx ,ry 0 0 0 (,w ,point))
				(:arc-to ,rx ,ry 0 0 0 (,rx ,(+ point ry)))
				(:line-to (,rx ,(- h ry)))
				(:arc-to ,rx ,ry 1 0 1 (0 ,h)))))))

(defun brace-make-path-right (w h r point)
  (let ((r (or r (/ w 3)))
		(point (or point (/ h 2))))
	(let ((rx (/ w 2))
		  (ry (min r (/ point 2) (/ (- h point) 2))))
	  (values point
			  `((:move-to (,w 0))
				(:arc-to ,rx ,ry 0 0 0 (,rx ,ry))
				(:line-to (,rx ,(- point ry)))
				(:arc-to ,rx ,ry 1 0 1 (0 ,point))
				(:arc-to ,rx ,ry 1 0 1 (,rx ,(+ point ry)))
				(:line-to (,rx ,(- h ry)))
				(:arc-to ,rx ,ry 0 0 0 (,w ,h)))))))

(defun brace-make-path-upper (w h r point)
  (let ((r (or r (/ h 3)))
		(point (or point (/ w 2))))
	(let ((ry (/ h 2))
		  (rx (min r (/ point 2) (/ (- w point) 2))))
	  (values point
			  `((:move-to (0 0))
				(:arc-to ,rx ,ry 0 0 0 (,rx ,ry))
				(:line-to (,(- point rx) ,ry))
				(:arc-to ,rx ,ry 1 0 1 (,point ,h))
				(:arc-to ,rx ,ry 1 0 1 (,(+ point rx) ,ry))
				(:line-to (,(- w rx) ,ry))
				(:arc-to ,rx ,ry 0 0 0 (,w 0)))))))

(defun brace-make-path-bottom (w h r point)
  (let ((r (or r (/ h 3)))
		(point (or point (/ w 2))))
	(let ((ry (/ h 2))
		  (rx (min r (/ point 2) (/ (- w point) 2))))
	  (values point
			  `((:move-to (0 ,h))
				(:arc-to ,rx ,ry 1 0 1 (,rx ,ry))
				(:line-to (,(- point rx) ,ry))
				(:arc-to ,rx ,ry 0 0 0 (,point 0))
				(:arc-to ,rx ,ry 0 0 0 (,(+ point rx) ,ry))
				(:line-to (,(- w rx) ,ry))
				(:arc-to ,rx ,ry 1 0 1 (,w ,h)))))))

;;------------------------------------------------------------------------------
;;
;; class brace
;;
;;------------------------------------------------------------------------------
(defclass brace (group)
  ((direction	:initform nil :initarg :direction)	; keyword(:upper,:bottom,:left,:right)
   (r			:initform nil :initarg :r)			; number
   (point		:initform nil :initarg :point)		; number
   (text		:initform nil :initarg :text)		; (or keyword string)
   (font		:initform nil :initarg :font)		; (or nil font-info)
   (stroke		:initform nil :initarg :stroke)		; (or nil stroke-info)
   (filter		:initform nil :initarg :filter))) ; (or nil keyword)
  
(defmethod initialize-instance :after ((brc brace) &rest initargs)
  (declare (ignore initargs))
  (with-slots (font stroke filter layer) brc
	(setf font   (make-font   (or font   *default-brace-font*   *default-font*)))
	(setf stroke (make-stroke (or stroke *default-brace-stroke* *default-stroke* :none)))
	(setf filter (if (eq filter :none)
					 nil
					 (or filter *default-brace-filter* *default-filter*)))
	(setf layer  (if (eq layer :none)
					 nil
					 (or layer *default-brace-layer* *default-layer*))))
  brc)

(defmethod check ((brc brace) canvas dict)
  (with-slots (direction r point text font stroke filter) brc
	(check-keywords direction :upper :bottom :left :right)
	(check-member   r       :nullable t :types number)
	(check-member   point   :nullable t :types number)
	(check-member   text    :nullable t :types (or keyword string))
	(check-object   font    canvas dict :nullable   t :class   font-info)
	(check-object   stroke  canvas dict :nullable nil :class stroke-info)
	(check-member filter    :nullable   t :types keyword)
	(when text
	  (setf text (fix-name text))))
  ;; this method must call super class' one.
  (call-next-method))


;; override of group::draw-group
(defmethod draw-group ((brc brace) writer)
  (let ((canvas (group-get-canvas brc)))
	(let ((width  (canvas-width  canvas))
		  (height (canvas-height canvas)))
	  (macrolet ((register-entity (entity)
				   `(check-and-draw-local-entity ,entity canvas writer)))
		(with-slots (direction r point text font stroke filter) brc
		  (multiple-value-bind (point data)
			  (case direction
				((:upper)  (brace-make-path-upper  width height r point))
				((:bottom) (brace-make-path-bottom width height r point))
				((:left)   (brace-make-path-left   width height r point))
				((:right)  (brace-make-path-right  width height r point)))
			;; draw brace
			(path data :stroke stroke :fill :none :filter filter)
			;; draw text
			(when text
			  (with-slots (size) font
				(multiple-value-bind (align pos)
					(case direction
					  ((:upper)  (values :center `(,point ,(+ height size 5))))
					  ((:bottom) (values :center `(,point -5)))
					  ((:left)   (values :left   `(,(+ width 5) ,(+ point (/ size 2)))))
					  ((:right)  (values :right  `(-5 ,(+ point (/ size 2))))))
				  (text pos text :align align :font font)))))))))
  nil)


;;------------------------------------------------------------------------------
;;
;; macro brace
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:brace
 |#
(defmacro brace (position direction width height
						&key pivot r point text font stroke layer filter id)
  `(register-entity (make-instance 'kaavio:brace
								   :position ,position :pivot ,pivot
								   :direction ,direction :width ,width :height ,height
								   :r ,r :point ,point
								   :text ,text :font ,font :stroke ,stroke
								   :link nil :layer ,layer :filter ,filter :id ,id)))


;;------------------------------------------------------------------------------
;;
;; macro with-brace-options
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:with-brace-options
 |#
(defmacro with-brace-options ((&key font stroke filter layer) &rest body)
  (labels ((impl (params acc)
			 (if (null params)
				 acc
				 (let ((value  (car  params))
					   (symbol (cadr params)))
				   (impl (cddr params)
						 (if (null value)
							 acc
							 (push (list symbol value) acc)))))))
	(let ((lst (impl (list font   '*default-brace-font*
						   stroke '*default-brace-stroke*
						   filter '*default-brace-filter*
						   layer  '*default-brace-layer*) nil)))
	  `(let ,lst
		 ,@body))))
