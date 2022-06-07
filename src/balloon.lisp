#|
#|ASD|#				(:file "balloon"                   :depends-on ("cl-diagram"
#|ASD|#																"arc"
#|ASD|#																"polygon"
#|ASD|#																"filter"
#|ASD|#																"text-shape"))
#|EXPORT|#				;balloon.lisp
 |#

(in-package :cl-diagram)

#|
#|EXPORT|#				:*default-balloon-round*
#|EXPORT|#				:*default-balloon-align*
#|EXPORT|#				:*default-balloon-valign*
#|EXPORT|#				:*default-balloon-margin*
#|EXPORT|#				:*default-balloon-filter*
 |#
(defparameter *default-balloon-round*  10)
(defparameter *default-balloon-align*  :center)
(defparameter *default-balloon-valign* :center)
(defparameter *default-balloon-margin* 10)
(defparameter *default-balloon-filter* nil)



(defun balloon-anchor-priority (w h pt pos)
  (with-point (x y) pt
	(let ((w2 (/ w 2))
		  (h2 (/ h 2)))
	  (cond
		((eq pos    :top) (if (<=  x w2)  5 15))
		((eq pos  :right) (if (<=  y h2) 25 35))
		((eq pos :bottom) (if (<  w2  x) 45 55))
		((eq pos   :left) (if (<  h2  y) 65 75))
		(t nil)))))

(defun balloon-make-path (w h r anchor pt pos)
  (let ((points nil)
		(pri (balloon-anchor-priority w h pt pos)))
	;; pathの先頭末尾を追加
	(push `( 0 (:move-to (,r 0))) points)
	(push '( 1 :absolute) points)
	(push '(99 :close-path) points)
	;; round 指定があるなら角の丸い部分を追加
	(when (< 0 r)
	  (push `(20 (:arc-to ,r ,r 1 0 1 (,w       ,r      ))) points)
	  (push `(40 (:arc-to ,r ,r 1 0 1 (,(- w r) ,h      ))) points)
	  (push `(60 (:arc-to ,r ,r 1 0 1 (0        ,(- h r)))) points)
	  (push `(80 (:arc-to ,r ,r 1 0 1 (,r       0       ))) points))
	;; upper side
	(when (= pri 5)
	  (push `(5 (:line-to ,anchor)     ) points)
	  (push `(6 (:line-to (,(/ w 3) 0))) points))
	(if (/= pri 15)
		(push `(10 (:line-to (,(- w r) 0))) points)
		(progn
		  (push `(10 (:line-to (,(* 2 (/ w 3)) 0))) points)
		  (push `(15 (:line-to ,anchor)     ) points)
		  (push `(16 (:line-to (,(- w r) 0))) points)))
	;; right side
	(when (= pri 25)
	  (push `(25 (:line-to ,anchor)      ) points)
	  (push `(26 (:line-to (,w ,(/ h 2)))) points))
	(if (/= pri 35)
		(push `(30 (:line-to (,w       ,(- h r)))) points)
		(progn
		  (push `(30 (:line-to (,w ,(/ h 2)))) points)
		  (push `(35 (:line-to ,anchor)      ) points)
		  (push `(36 (:line-to (,w ,(- h r)))) points)))
	;; bottom side
	(when (= pri 45)
	  (push `(45 (:line-to ,anchor)      ) points)
	  (push `(46 (:line-to (,(* 2 (/ w 3)) ,h))) points))
	(if (/= pri 55)
		(push `(50 (:line-to (,r ,h))) points)
		(progn
		  (push `(50 (:line-to (,(/ w 3) ,h))) points)
		  (push `(55 (:line-to ,anchor)      ) points)
		  (push `(56 (:line-to (,r       ,h))) points)))
	;; left side
	(when (= pri 65)
	  (push `(65 (:line-to ,anchor)     ) points)
	  (push `(66 (:line-to (0 ,(/ h 2)))) points))
	(if (/= pri 75)
		(push `(70 (:line-to (0 ,r))) points)
		(progn
		  (push `(70 (:line-to (0 ,(/ h 2)))) points)
		  (push `(75 (:line-to ,anchor)     ) points)
		  (push `(76 (:line-to (0 ,r      ))) points)))
	;; 最後にソートして先頭の優先順位を除去して返す
	(mapcar (lambda (lst) (cadr lst))
			(sort points (lambda (e1 e2)
						   (< (car e1) (car e2)))))))


;;------------------------------------------------------------------------------
;;
;; class balloon
;;
;;------------------------------------------------------------------------------
(defclass balloon (text-shape)
  ((round	:initform nil :initarg :round)    ; number
   (anchor	:initform nil :initarg :anchor)   ; point
   (filter	:initform nil :initarg :filter))) ; (or nil keyword)
  
(defmethod initialize-instance :after ((bln balloon) &rest initargs)
  (declare (ignore initargs))
  (with-slots (filter) bln
	(setf filter (or filter *default-balloon-filter* *default-shape-filter* *default-filter*)))
  bln)
   
(defmethod check ((bln balloon) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (filter) bln
	(check-member filter    :nullable   t :types keyword))
  nil)

;; override of group::draw-group
(defmethod draw-group ((box balloon) writer)
  (let* ((canvas (group-get-canvas box))
		 (width  (canvas-width  canvas))
		 (height (canvas-height canvas)))
	(macrolet ((register-entity (entity)
				 `(check-and-draw-local-entity ,entity canvas writer)))
	  (with-slots (round anchor fill stroke filter) box
		(labels ((abs2rel (pt)
				   (let ((topleft (canvas-topleft canvas)))
					 (make-point (- (point-x pt) (point-x topleft))
								 (- (point-y pt) (point-y topleft))))))
		  ;; draw box
		  (multiple-value-bind (pt pos)
			  (rectangle-connect-point-C (shape-center box) width height anchor)
			(let ((points (balloon-make-path width height (or round 0)
											(abs2rel anchor) (abs2rel pt) pos)))
			  (path points :fill fill :stroke stroke :filter filter)))))))
  ;; draw text
  (call-next-method))

;; no override.
;(defmethod text-shape-calc-size ((box balloon))
;  (call-next-method))

;; no override.
;(defmethod text-shape-paragraph-area ((box balloon))
;  (call-next-method))

;;------------------------------------------------------------------------------
;;
;; macro balloon
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:balloon
 |#
(defmacro balloon (center text anchor &key round width height align valign
										   font fill stroke margin link rotate layer id filter contents)
  (let ((code `(register-entity (make-instance 'balloon
											   :anchor ,anchor
											   :round  (or ,round  *default-balloon-round*)
											   :center ,center
											   :width ,width :height ,height
											   :text ,text :font ,font
											   :align  (or ,align  *default-balloon-align*)
											   :valign (or ,valign *default-balloon-valign*)
											   :margin (or ,margin *default-balloon-margin*)
											   :fill ,fill :stroke ,stroke
											   :link ,link :rotate ,rotate
											   :filter ,filter :layer ,layer :id ,id))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (diagram:shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))

