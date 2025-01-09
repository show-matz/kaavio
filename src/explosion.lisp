#|
#|ASD|#                (:file "explosion"                 :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "polygon"
#|ASD|#                                                                "filter"
#|ASD|#                                                                "text-shape"))
#|EXPORT|#                ;explosion.lisp
 |#

(in-package :kaavio)

#|
#|EXPORT|#                :*default-explosion-font*
#|EXPORT|#                :*default-explosion-fill*
#|EXPORT|#                :*default-explosion-stroke*
#|EXPORT|#                :*default-explosion-filter*
#|EXPORT|#                :*default-explosion-layer*
 |#
(defparameter *default-explosion-font*         nil)
(defparameter *default-explosion-fill*         nil)
(defparameter *default-explosion-stroke*       nil)
(defparameter *default-explosion-filter*       nil)
(defparameter *default-explosion-layer*        nil)


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
   (filter  :initform nil :initarg :filter))) ; (or nil keyword)
  
(defmethod initialize-instance :after ((exp explosion) &rest initargs)
  (declare (ignore initargs))
  (with-slots (layer filter) exp
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-explosion-layer* *default-layer*)))
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-explosion-filter* *default-filter*))))
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

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro explosion1
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{explosion1}} position width height text ${KEY} pivot font fill stroke link rotate layer id filter contents
;;
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　${{TODO}{まだ記述されていません。}}
;;
;;${NO_SEE_ALSO}
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :explosion1
 |#
(defmacro explosion1 (position width height text
                         &key pivot font fill stroke link rotate layer id filter contents)
  (let ((code `(register-entity (make-instance 'explosion
                                               :pattern 1 :position ,position :pivot ,pivot
                                               :width ,width :height ,height
                                               :text ,text
                                               :align  :center :valign :center
                                               :font   (or ,font   *default-explosion-font*)
                                               :fill   (or ,fill   *default-explosion-fill*)
                                               :stroke (or ,stroke *default-explosion-stroke*)
                                               :link ,link  :rotate ,rotate
                                               :filter ,filter :layer ,layer :id ,id))))
    (if (null contents)
        code
        (let ((g-obj (gensym "OBJ")))
          `(let* ((,g-obj ,code)
                  (canvas (kaavio:shape-get-subcanvas ,g-obj)))
             (declare (special canvas))
             ,@contents)))))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro explosion2
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{explosion2}} position width height text ${KEY} pivot font fill stroke link rotate layer id filter contents
;;
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　${{TODO}{まだ記述されていません。}}
;;
;;${NO_SEE_ALSO}
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :explosion2
 |#
(defmacro explosion2 (position width height text
                         &key pivot font fill stroke link rotate layer id filter contents)
  (let ((code `(register-entity (make-instance 'explosion
                                               :pattern 2 :position ,position :pivot ,pivot
                                               :width ,width :height ,height
                                               :text ,text
                                               :align  :center :valign :center
                                               :font   (or ,font   *default-explosion-font*)
                                               :fill   (or ,fill   *default-explosion-fill*)
                                               :stroke (or ,stroke *default-explosion-stroke*)
                                               :link ,link  :rotate ,rotate
                                               :filter ,filter :layer ,layer :id ,id))))
    (if (null contents)
        code
        (let ((g-obj (gensym "OBJ")))
          `(let* ((,g-obj ,code)
                  (canvas (kaavio:shape-get-subcanvas ,g-obj)))
             (declare (special canvas))
             ,@contents)))))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-explosion-options
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-explosion-options}} (${KEY} font fill stroke filter layer) ${BODY} body
;;
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　${{TODO}{まだ記述されていません。}}
;;
;;${NO_SEE_ALSO}
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-explosion-options
 |#
(defmacro with-explosion-options ((&key font fill stroke filter layer) &rest body)
  (labels ((impl (params acc)
             (if (null params)
                 acc
                 (let ((value  (car  params))
                       (symbol (cadr params)))
                   (impl (cddr params)
                         (if (null value)
                             acc
                             (push (list symbol value) acc)))))))
    (let ((lst (impl (list font   '*default-explosion-font*
                           fill   '*default-explosion-fill*
                           stroke '*default-explosion-stroke*
                           filter '*default-explosion-filter*
                           layer  '*default-explosion-layer*) nil)))
      `(let ,lst
         ,@body))))
