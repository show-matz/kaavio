#|
#|ASD|#             (:file "group"                     :depends-on ("kaavio"
#|ASD|#                                                             "canvas"
#|ASD|#                                                             "shape"
#|ASD|#                                                             "rectangle"
#|ASD|#                                                             "writer"))
#|EXPORT|#              ;group.lisp
 |#


(in-package :kaavio)

;;------------------------------------------------------------------------------
;;
;; abstract class group
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#              :group
#|EXPORT|#              :group-get-canvas
#|EXPORT|#              :draw-group
 |#
(defclass group (shape)
  ((position    :initform nil :initarg :position)    ; point
   (pivot       :initform :CC :initarg :pivot)       ; keyword
   (width       :initform 0   :initarg :width)       ; number
   (height      :initform 0   :initarg :height)))    ; number

(defgeneric group-get-canvas (grp))
(defgeneric draw-group (grp writer))



(defmethod initialize-instance :after ((grp group) &rest initargs)
  (declare (ignore initargs))
  (with-slots (pivot) grp
    (setf pivot  (or pivot :CC)))
  grp)

(defmethod check ((grp group) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (position pivot width height) grp
    (check-member pivot  :nullable nil :types keyword)
    (check-member width  :nullable nil :types number)
    (check-member height :nullable nil :types number)
    (setf position (canvas-fix-point canvas position)))
  nil)

(defmethod attribute-width ((grp group))
  (slot-value grp 'width))

(defmethod attribute-height ((grp group))
  (slot-value grp 'height))

(defmethod attribute-center ((grp group))
  (with-slots (position pivot width height) grp
    (shape-calc-center-using-pivot position pivot width height)))

;;MEMO : use impelementation of shape...
;;(defmethod shape-connect-point ((grp group) type1 type2 arg) ...)
  
;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((grp group)) ...)

(defmethod entity-composition-p ((grp group))
  t)  

(defmethod draw-entity ((grp group) writer)
    (pre-draw   grp writer)
    (draw-group grp writer)
    (post-draw  grp writer)
  nil)
                      

(defmethod group-get-canvas ((grp group))
  (make-canvas (attribute-topleft grp)
               (attribute-width   grp)
               (attribute-height  grp)))


#|
#|EXPORT|#              :draw-canvas-frame
 |#
(defun draw-canvas-frame (canvas writer &key (color :blue))
  (let ((*default-fill*   (make-fill   :color color :opacity 0.2))
        (*default-stroke* (make-stroke :color color :dasharray '(2 2))))
    (macrolet ((register-entity (entity)
                 `(check-and-draw-local-entity ,entity canvas writer)))
      (with-canvas (center width height) canvas
        (rectangle center width height)))))

