#|
#|ASD|#             (:file "cross"                     :depends-on ("kaavio"
#|ASD|#                                                             "constants"
#|ASD|#                                                             "canvas"
#|ASD|#                                                             "point"
#|ASD|#                                                             "shape"
#|ASD|#                                                             "stroke-info"
#|ASD|#                                                             "link-info"
#|ASD|#                                                             "filter"
#|ASD|#                                                             "writer"))
#|EXPORT|#              ;cross.lisp
 |#


(in-package :kaavio)

#|
#|EXPORT|#              :*default-cross-fill*
#|EXPORT|#              :*default-cross-stroke*
#|EXPORT|#              :*default-cross-filter*
#|EXPORT|#              :*default-cross-layer*
 |#
(defparameter *default-cross-fill*          nil)
(defparameter *default-cross-stroke*        nil)
(defparameter *default-cross-filter*        nil)
(defparameter *default-cross-layer*         nil)

;;------------------------------------------------------------------------------
;;
;; class cross
;;
;;------------------------------------------------------------------------------
(defclass cross (shape)
  ((position     :initform nil :initarg :position)     ; point
   (pivot        :initform :CC :initarg :pivot)        ; keyword
   (width        :initform   0 :initarg :width)        ; number
   (height       :initform   0 :initarg :height)       ; number
   (size         :initform   0 :initarg :size)         ; number
   (size-v       :initform   0 :initarg :size-v)       ; number
   (intersection :initform   0 :initarg :intersection) ; point
   (fill         :initform nil :initarg :fill)         ; (or nil fill-info)
   (stroke       :initform nil :initarg :stroke)       ; (or nil stroke-info)
   (filter       :initform nil :initarg :filter)))     ; (or nil keyword)

(defmethod initialize-instance :after ((crs cross) &rest initargs)
  (declare (ignore initargs))
  (with-slots (pivot size size-v intersection fill stroke filter layer) crs
    (setf pivot  (or pivot :CC))
    (setf size-v (or size-v size))
    (setf intersection  (or intersection (make-point 0 0)))
    (setf fill   (make-fill   (or fill   *default-fill*   :none)))
    (setf stroke (make-stroke (or stroke *default-stroke* :none)))
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-cross-filter* *default-filter*)))
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-cross-layer* *default-layer*))))
  crs)

(defmethod check ((crs cross) canvas dict)
  (declare (ignorable dict))
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (position pivot width height size size-v intersection fill stroke filter) crs
    (check-member pivot     :nullable nil :types keyword)
    (check-member width     :nullable nil :types number)
    (check-member height    :nullable nil :types number)
    (check-member size      :nullable nil :types number)
    (check-member size-v    :nullable nil :types number)
    (check-member intersection :nullable nil :types cons)
    (check-object fill      canvas dict :nullable nil :class   fill-info)
    (check-object stroke    canvas dict :nullable nil :class stroke-info)
    (check-member filter    :nullable   t :types keyword)
    (setf position (canvas-fix-point canvas position)))
  nil)

(defmethod attribute-width ((crs cross))
  (slot-value crs 'width))

(defmethod attribute-height ((crs cross))
  (slot-value crs 'height))

(defmethod attribute-center ((crs cross))
  (with-slots (position pivot width height) crs
    (shape-calc-center-using-pivot position pivot width height)))


;;MEMO : use impelementation of shape...
;;(defmethod shape-connect-point ((crs cross) type1 type2 arg) ...)

;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((crs cross)) ...)

;;MEMO : use impelementation of shape...
;;(defmethod entity-composition-p ((crs cross)) ...)
  
(defmethod draw-entity ((crs cross) writer)
  (pre-draw crs writer)
  (with-slots (width height size size-v intersection fill stroke filter) crs
    (let* ((center (attribute-center crs))
           (cc   (point+ center intersection))
           (hs/2 (/ size   2))
           (vs/2 (/ size-v 2))
           (v1   (- (point-y center) (/ height 2)))
           (v2   (- (point-y cc) hs/2))
           (v3   (+ (point-y cc) hs/2))
           (v4   (+ (point-y center) (/ height 2)))
           (h1   (- (point-x center) (/ width 2)))
           (h2   (- (point-x cc) vs/2))
           (h3   (+ (point-x cc) vs/2))
           (h4   (+ (point-x center) (/ width 2)))
           (id (and (not (entity-composition-p crs)) (slot-value crs 'id))))
      (writer-write writer
                    "<path "
                    (write-when (keywordp id) "id='" id "' ")
                    (to-property-strings fill)
                    (to-property-strings stroke)
                    "d='M " h2 " " v1 " "
                       "V " v2 " H " h1 " "
                       "V " v3 " H " h2 " "
                       "V " v4 " H " h3 " "
                       "V " v3 " H " h4 " "
                       "V " v2 " H " h3 " "
                       "V " v1 " z' "
                    (write-when filter "filter='url(#" it ")' ")
                    "/>")))
  (post-draw crs writer)
  nil)


;;------------------------------------------------------------------------------
;;
;; macro cross
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#              :cross
 |#
(defmacro cross (position width height size
                        &key pivot size-v intersection fill stroke filter rotate link layer id)
  `(register-entity (make-instance 'kaavio:cross
                                   :position ,position :pivot ,pivot
                                   :width ,width :height ,height
                                   :size ,size :size-v ,size-v
                                   :intersection ,intersection
                                   :fill   (or ,fill   *default-cross-fill*)
                                   :stroke (or ,stroke *default-cross-stroke*)
                                   :filter ,filter :rotate ,rotate
                                   :link ,link :id ,id :layer ,layer)))

;;------------------------------------------------------------------------------
;;
;; macro with-cross-options
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#              :with-cross-options
 |#
(defmacro with-cross-options ((&key fill stroke filter layer) &rest body)
  (labels ((impl (params acc)
             (if (null params)
                 acc
                 (let ((value  (car  params))
                       (symbol (cadr params)))
                   (impl (cddr params)
                         (if (null value)
                             acc
                             (push (list symbol value) acc)))))))
    (let ((lst (impl (list fill   '*default-cross-fill*
                           stroke '*default-cross-stroke*
                           filter '*default-cross-filter*
                           layer  '*default-cross-layer*) nil)))
      `(let ,lst
         ,@body))))
