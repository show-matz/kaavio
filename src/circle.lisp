#|
#|ASD|#                (:file "circle"                    :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "mathutil"
#|ASD|#                                                                "canvas"
#|ASD|#                                                                "point"
#|ASD|#                                                                "shape"
#|ASD|#                                                                "stroke-info"
#|ASD|#                                                                "link-info"
#|ASD|#                                                                "filter"
#|ASD|#                                                                "writer"))
#|EXPORT|#                ;circle.lisp
 |#


(in-package :kaavio)

;;------------------------------------------------------------------------------
;;
;; utility functions
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#                :circle-connect-point
 |#
(defun circle-connect-point (center radius type1 type2 arg)
  (declare (ignore type1))
  (let ((cx (point-x center))
        (cy (point-y center)))
    (if (eq type2 :center)
        (with-point (px py) arg
                    (let ((x (* radius (math/cos4 px py cx cy)))
                          (y (* radius (math/sin4 px py cx cy))))
                      (make-point (- cx x) (- cy y) :absolute)))
        (let ((degree (ecase type2
                        ((:right)  (+   0 (* arg 30)))
                        ((:bottom) (-  90 (* arg 30)))
                        ((:left)   (- 180 (* arg 30)))
                        ((:top)    (+ 270 (* arg 30))))))
          (when (< degree 0)
            (incf degree 360))
          (let ((x (* radius (math/cos1 degree)))
                (y (* radius (math/sin1 degree))))
            (make-point (+ cx x) (+ cy y) :absolute))))))


;;------------------------------------------------------------------------------
;;
;; class circle
;;
;;------------------------------------------------------------------------------
(defclass circle (shape)
  ((position    :initform nil :initarg :position)    ; point
   (pivot       :initform :CC :initarg :pivot)       ; keyword
   (radius      :initform   0 :initarg :radius)      ; number
   (fill        :initform nil :initarg :fill)        ; (or nil fill-info)
   (stroke      :initform nil :initarg :stroke)      ; (or nil stroke-info)
   (filter      :initform nil :initarg :filter)))    ; (or nil keyword)

(defmethod initialize-instance :after ((ent circle) &rest initargs)
  (declare (ignore initargs))
  (with-slots (pivot fill stroke filter) ent
    (setf pivot  (or pivot :CC))
    (setf fill   (make-fill   (or fill   *default-fill*   :none)))
    (setf stroke (make-stroke (or stroke *default-stroke* :none)))
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-filter*))))
  ent)

(defmethod check ((shp circle) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (position pivot radius fill stroke filter layer) shp
    (check-member pivot    :nullable nil :types keyword)
    (check-member radius   :nullable nil :types number)
    (check-object fill      canvas dict :nullable nil :class   fill-info)
    (check-object stroke    canvas dict :nullable nil :class stroke-info)
    (check-member filter   :nullable   t :types keyword)
    (setf filter (if (eq filter :none) nil filter))
    (setf position (canvas-fix-point canvas position))
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-layer*))))
  nil)

(defmethod attribute-width ((shp circle))
  (* 2 (slot-value shp 'radius)))

(defmethod attribute-height ((shp circle))
  (* 2 (slot-value shp 'radius)))

(defmethod attribute-center ((shp circle))
  (with-slots (position pivot radius) shp
    (shape-calc-center-using-pivot position pivot (* 2 radius) (* 2 radius))))


(defmethod shape-connect-point ((shp circle) type1 type2 arg)
  (circle-connect-point (attribute-center shp)
                        (slot-value shp 'radius) type1 type2 arg))
  
;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((shp circle)) ...)

;;MEMO : use impelementation of shape...
;;(defmethod entity-composition-p ((shp circle)) ...)
  
(defmethod draw-entity ((shp circle) writer)
  (with-slots (radius fill stroke filter) shp
    (let ((id (and (not (entity-composition-p shp))
                   (slot-value shp 'id)))
          (center (attribute-center shp)))
      (pre-draw shp writer)
      (writer-write writer
                    "<circle "
                    (write-when (keywordp id) "id='" id "' ")
                    "cx='" (point-x center) "' "
                    "cy='" (point-y center) "' "
                    "r='" radius "' "
                    (to-property-strings fill)
                    (to-property-strings stroke)
                    (write-when filter "filter='url(#" it ")' ")
                    "/>")
      (post-draw shp writer)))
  nil)


;;------------------------------------------------------------------------------
;;
;; macro circle
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#                :circle
 |#
(defmacro circle (position radius
                  &key pivot fill stroke link layer id filter contents)
  (let ((code `(register-entity (make-instance 'kaavio:circle
                                               :position ,position
                                               :pivot ,pivot :radius ,radius
                                               :fill ,fill :stroke ,stroke :link ,link
                                               :filter ,filter :layer ,layer :id ,id))))
    (if (null contents)
        code
        (let ((g-obj (gensym "OBJ")))
          `(let* ((,g-obj ,code)
                  (canvas (kaavio:shape-get-subcanvas ,g-obj)))
             (declare (special canvas))
             ,@contents)))))

