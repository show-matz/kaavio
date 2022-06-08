#|
#|ASD|#				(:file "explosion"                 :depends-on ("cl-diagram"
#|ASD|#																"polygon"
#|ASD|#																"filter"
#|ASD|#																"text-shape"))
#|EXPORT|#				;explosion.lisp
 |#

(in-package :cl-diagram)


(defun explosion-get-points (pattern w h)
  (cond
	((= pattern 1) `((,(* w 0.02) ,(* h 0.10))
					 (,(* w 0.22) ,(* h 0.35))
					 (,(* w 0.01) ,(* h 0.39))
					 (,(* w 0.18) ,(* h 0.54))
					 (,(* w 0.01) ,(* h 0.67))
					 (,(* w 0.27) ,(* h 0.65))
					 (,(* w 0.23) ,(* h 0.80))
					 (,(* w 0.36) ,(* h 0.73))
					 (,(* w 0.39) ,(* h 0.99))
					 (,(* w 0.49) ,(* h 0.69))
					 (,(* w 0.62) ,(* h 0.91))
					 (,(* w 0.66) ,(* h 0.67))
					 (,(* w 0.84) ,(* h 0.83))
					 (,(* w 0.78) ,(* h 0.59))
					 (,(* w 0.99) ,(* h 0.61))
					 (,(* w 0.81) ,(* h 0.49))
					 (,(* w 0.98) ,(* h 0.38))
					 (,(* w 0.77) ,(* h 0.34))
					 (,(* w 0.85) ,(* h 0.21))
					 (,(* w 0.65) ,(* h 0.25))
					 (,(* w 0.67) ,(* h 0.00))
					 (,(* w 0.50) ,(* h 0.27))
					 (,(* w 0.39) ,(* h 0.12))
					 (,(* w 0.34) ,(* h 0.29))))
	((= pattern 2) `((,(* w 0.21) ,(* h 0.17))
					 (,(* w 0.25) ,(* h 0.36))
					 (,(* w 0.05) ,(* h 0.38))
					 (,(* w 0.18) ,(* h 0.54))
					 (,(* w 0.00) ,(* h 0.59))
					 (,(* w 0.15) ,(* h 0.71))
					 (,(* w 0.06) ,(* h 0.82))
					 (,(* w 0.22) ,(* h 0.84))
					 (,(* w 0.23) ,(* h 1.00))
					 (,(* w 0.35) ,(* h 0.83))
					 (,(* w 0.40) ,(* h 0.91))
					 (,(* w 0.46) ,(* h 0.80))
					 (,(* w 0.54) ,(* h 0.87))
					 (,(* w 0.56) ,(* h 0.73))
					 (,(* w 0.69) ,(* h 0.80))
					 (,(* w 0.68) ,(* h 0.66))
					 (,(* w 0.87) ,(* h 0.72))
					 (,(* w 0.76) ,(* h 0.57))
					 (,(* w 0.85) ,(* h 0.52))
					 (,(* w 0.78) ,(* h 0.44))
					 (,(* w 1.00) ,(* h 0.31))
					 (,(* w 0.76) ,(* h 0.30))
					 (,(* w 0.83) ,(* h 0.14))
					 (,(* w 0.67) ,(* h 0.27))
					 (,(* w 0.68) ,(* h 0.00))
					 (,(* w 0.53) ,(* h 0.20))
					 (,(* w 0.45) ,(* h 0.09))
					 (,(* w 0.40) ,(* h 0.30))))
	 (t nil)))
	
  

;;------------------------------------------------------------------------------
;;
;; class explosion
;;
;;------------------------------------------------------------------------------
(defclass explosion (text-shape)
  ((pattern :initform nil :initarg :pattern)  ; number
   (filter	:initform nil :initarg :filter))) ; (or nil keyword)
  
(defmethod initialize-instance :after ((exp explosion) &rest initargs)
  (declare (ignore initargs))
  (with-slots (filter) exp
	(setf filter (or filter *default-shape-filter* *default-filter*)))
  exp)
   
(defmethod check ((exp explosion) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (pattern filter) exp
	(check-member pattern :nullable nil :types number)
	(check-member filter  :nullable   t :types keyword))
  nil)

;; override of group::draw-group
(defmethod draw-group ((exp explosion) writer)
  (let* ((canvas (group-get-canvas exp))
		 (width  (canvas-width  canvas))
		 (height (canvas-height canvas)))
	(macrolet ((register-entity (entity)
				 `(check-and-draw-local-entity ,entity canvas writer)))
	  (with-slots (pattern fill stroke filter) exp
		;; draw 
		(polygon (explosion-get-points pattern width height)
				 :stroke stroke :fill fill :filter filter))))
  ;; draw text
  (call-next-method))

;; no override.
;(defmethod text-shape-calc-size ((exp explosion))
;  (call-next-method))

;; no override.
;(defmethod text-shape-paragraph-area ((exp explosion))
;  (call-next-method))

;;------------------------------------------------------------------------------
;;
;; macro explosion
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:explosion1
 |#
(defmacro explosion1 (center width height text
						 &key font fill stroke link rotate layer id filter)
  `(register-entity (make-instance 'explosion
								   :pattern 1 :center ,center
								   :width ,width :height ,height
								   :text ,text :font ,font
								   :align  :center :valign :center
								   :fill ,fill :stroke ,stroke :link ,link 
								   :rotate ,rotate :layer ,layer :filter ,filter :id ,id)))

#|
#|EXPORT|#				:explosion2
 |#
(defmacro explosion2 (center width height text
						 &key font fill stroke link rotate layer id filter)
  `(register-entity (make-instance 'explosion
								   :pattern 2 :center ,center
								   :width ,width :height ,height
								   :text ,text :font ,font
								   :align  :center :valign :center
								   :fill ,fill :stroke ,stroke :link ,link 
								   :rotate ,rotate :layer ,layer :filter ,filter :id ,id)))

