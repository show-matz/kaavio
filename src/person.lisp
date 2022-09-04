#|
#|ASD|#				(:file "person"                    :depends-on ("cl-diagram"
#|ASD|#																"constants"
#|ASD|#																"path"))
#|EXPORT|#				;person.lisp
 |#

(in-package :cl-diagram)

#|
#|EXPORT|#				:*default-person-fill*
#|EXPORT|#				:*default-person-stroke*
#|EXPORT|#				:*default-person-filter*
#|EXPORT|#				:*default-person-layer*
 |#
(defparameter *default-person-fill*         nil)
(defparameter *default-person-stroke*       nil)
(defparameter *default-person-filter*       nil)
(defparameter *default-person-layer*        nil)


;;------------------------------------------------------------------------------
;;
;; class person
;;
;;------------------------------------------------------------------------------
(defclass person (group)
  ((label		:initform nil :initarg :label)		; (or nil label-info)
   (fill		:initform nil :initarg :fill)		; (or nil fill-info)
   (stroke		:initform nil :initarg :stroke)		; (or nil stroke-info)
   (filter		:initform nil :initarg :filter)))	; (or nil keyword)
  
(defmethod initialize-instance :after ((prsn person) &rest initargs)
  (declare (ignore initargs))
  (with-slots (label fill stroke filter layer) prsn
	(setf label  (and label (make-label label)))
	(setf fill   (make-fill   (or fill   *default-person-fill*   *default-fill*)))
	(setf stroke (make-stroke (or stroke *default-person-stroke* *default-stroke* :none)))
	(setf filter (if (eq filter :none)
					 nil
					 (or filter *default-person-filter* *default-shape-filter*)))
	(setf layer  (if (eq layer :none)
					 nil
					 (or layer *default-person-layer* *default-layer*))))
  prsn)

(defmethod check ((prsn person) canvas dict)
  (with-slots (label fill stroke filter) prsn
	(check-object   label   canvas dict :nullable t   :class  label-info)
	(check-object   fill    canvas dict :nullable nil :class   fill-info)
	(check-object   stroke  canvas dict :nullable nil :class stroke-info)
	(check-member   filter  :nullable   t :types keyword))
  ;; this method must call super class' one.
  (call-next-method))


;; override of group::draw-group
(defmethod draw-group ((prsn person) writer)
  (let ((canvas (group-get-canvas prsn)))
	(with-canvas (cc w h) canvas
	  (macrolet ((register-entity (entity)
				   `(check-and-draw-local-entity ,entity canvas writer)))
		(with-slots (label fill stroke filter) prsn
		  ;; draw person
		  (let ((w/2 (/ w 2))
				(h/2 (/ h 2)))
			(path `((:move-to ,(xy+ cc (- w/2) h/2))
					(:arc-to ,w/2 ,h/2 0 1 1 ,(xy+ cc w/2 h/2))
					(:line-to ,(xy+ cc (- w/2) h/2))
					(:move-to ,cc)
					(:arc-to ,w/2 ,w/2 0 0 1 ,(y+ cc (- h/2)))
					(:arc-to ,w/2 ,w/2 0 0 1 ,cc))
				  :fill fill :stroke stroke :filter filter))
		  ;; draw label
		  (when label
			(draw-label label prsn writer))))))
  nil)


;;------------------------------------------------------------------------------
;;
;; macro person
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:person
 |#
(defmacro person (center size
						&key fill stroke label link rotate layer filter id)
  `(register-entity (make-instance 'diagram:person
								   :label ,label :fill ,fill
								   :stroke ,stroke :filter ,filter
					#| group  |#   :center ,center :width ,size :height ,(* 2 size)
					#| shape  |#   :link ,link :rotate ,rotate
					#| entity |#   :id ,id :layer ,layer)))


;;------------------------------------------------------------------------------
;;
;; macro with-person-options
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:with-person-options
 |#
(defmacro with-person-options ((&key fill stroke filter layer) &rest body)
  (labels ((impl (params acc)
			 (if (null params)
				 acc
				 (let ((value  (car  params))
					   (symbol (cadr params)))
				   (impl (cddr params)
						 (if (null value)
							 acc
							 (push (list symbol value) acc)))))))
	(let ((lst (impl (list fill   '*default-person-fill*
						   stroke '*default-person-stroke*
						   filter '*default-person-filter*
						   layer  '*default-person-layer*) nil)))
	  `(let ,lst
		 ,@body))))
