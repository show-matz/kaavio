#|
#|ASD|#                (:file "cylinder"                  :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "path"
#|ASD|#                                                                "filter"
#|ASD|#                                                                "text-shape"))
#|EXPORT|#                ;cylinder.lisp
 |#

(in-package :kaavio)

#|
#|EXPORT|#                :*default-cylinder-depth*
#|EXPORT|#                :*default-cylinder-align*
#|EXPORT|#                :*default-cylinder-valign*
#|EXPORT|#                :*default-cylinder-margin*
#|EXPORT|#                :*default-cylinder-font*
#|EXPORT|#                :*default-cylinder-fill*
#|EXPORT|#                :*default-cylinder-stroke*
#|EXPORT|#                :*default-cylinder-filter*
#|EXPORT|#                :*default-cylinder-layer*
 |#
(defparameter *default-cylinder-depth*        nil)
(defparameter *default-cylinder-align*    :center)
(defparameter *default-cylinder-valign*   :center)
(defparameter *default-cylinder-margin*        10)
(defparameter *default-cylinder-font*         nil)
(defparameter *default-cylinder-fill*         nil)
(defparameter *default-cylinder-stroke*       nil)
(defparameter *default-cylinder-filter*       nil)
(defparameter *default-cylinder-layer*        nil)

;;------------------------------------------------------------------------------
;;
;; class cylinder
;;
;;------------------------------------------------------------------------------
(defclass cylinder (text-shape)
  ((depth  :initform nil :initarg :depth)    ; number
   (filter :initform nil :initarg :filter))) ; (or nil keyword)
  
(defmethod initialize-instance :after ((cyl cylinder) &rest initargs)
  (declare (ignore initargs))
  (with-slots (filter layer) cyl
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-cylinder-filter* *default-filter*)))
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-cylinder-layer* *default-layer*))))
  cyl)
   
(defmethod check ((cyl cylinder) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (depth filter height) cyl
    (setf depth (or depth (/ height 5)))
    (check-member depth  :nullable nil :types number)
    (check-member filter :nullable   t :types keyword))
  nil)

;; override of group::draw-group
(defmethod draw-group ((cyl cylinder) writer)
  (let* ((canvas (group-get-canvas cyl))
         (w      (canvas-width  canvas))
         (h      (canvas-height canvas)))
    (macrolet ((register-entity (entity)
                 `(check-and-draw-local-entity ,entity canvas writer)))
      (with-slots (depth fill stroke filter) cyl
        (writer-write writer "<g " (to-property-strings stroke) ">")
        (writer-incr-level writer)
        (let ((*mute-stroke* t))
          ;; draw 
          (path `((:move-to (0  0))
                  (:line-to (0 ,h))
                  (:arc-to ,(/ w 2) ,(/ depth 2) 0 0 0 (,w ,h))
                  (:line-to (,w 0))
                  (:arc-to ,(/ w 2) ,(/ depth 2) 0 0 0 (0 0))) :fill fill :filter filter)
          (path `((:move-to (0  0))
                  (:arc-to ,(/ w 2) ,(/ depth 2) 0 0 0 (,w 0))) :fill :none))
        (writer-decr-level writer)
        (writer-write writer "</g>"))))
  ;; draw text
  (call-next-method))

;; no override.
;(defmethod text-shape-calc-size ((cyl cylinder))
;  (call-next-method))

;; no override.
;(defmethod text-shape-paragraph-area ((cyl cylinder))
;  (call-next-method))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro cylinder
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{cylinder}} position width height text ${KEY} pivot depth align valign margin font fill stroke link rotate layer id filter contents
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
#|EXPORT|#                :cylinder
 |#
(defmacro cylinder (position width height text
                         &key pivot depth align valign margin
                              font fill stroke link rotate layer id filter contents)
  (let ((code `(register-entity (make-instance 'cylinder
                                               :position ,position :pivot ,pivot :text ,text
                                               :width ,width :height ,height
                                               :depth  (or ,depth  *default-cylinder-depth*)
                                               :align  (or ,align  *default-cylinder-align*)
                                               :valign (or ,valign *default-cylinder-valign*)
                                               :margin (or ,margin *default-cylinder-margin*)
                                               :font   (or ,font   *default-cylinder-font*)
                                               :fill   (or ,fill   *default-cylinder-fill*)
                                               :stroke (or ,stroke *default-cylinder-stroke*)
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
;;#### macro with-cylinder-options
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-cylinder-options}} (${KEY} depth align valign margin font fill stroke filter layer) ${BODY} body
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
#|EXPORT|#                :with-cylinder-options
 |#
(defmacro with-cylinder-options ((&key depth align valign margin
                                       font fill stroke filter layer) &rest body)
  (labels ((impl (params acc)
             (if (null params)
                 acc
                 (let ((value  (car  params))
                       (symbol (cadr params)))
                   (impl (cddr params)
                         (if (null value)
                             acc
                             (push (list symbol value) acc)))))))
    (let ((lst (impl (list depth  '*default-cylinder-depth*
                           align  '*default-cylinder-align*
                           valign '*default-cylinder-valign*
                           margin '*default-cylinder-margin*
                           font   '*default-cylinder-font*
                           fill   '*default-cylinder-fill*
                           stroke '*default-cylinder-stroke*
                           filter '*default-cylinder-filter*
                           layer  '*default-cylinder-layer*) nil)))
      `(let ,lst
         ,@body))))
