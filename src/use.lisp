#|
#|ASD|#             (:file "use"                       :depends-on ("kaavio"
#|ASD|#                                                             "constants"
#|ASD|#                                                             "defgroup"
#|ASD|#                                                             "canvas"
#|ASD|#                                                             "point"
#|ASD|#                                                             "shape"
#|ASD|#                                                             "writer"))
#|EXPORT|#              ;use.lisp
 |#

(in-package :kaavio)


;;-------------------------------------------------------------------------------
;;
;; class use
;;
;;-------------------------------------------------------------------------------
(defclass use (shape)
  ((ref       :initform nil :initarg :ref)        ; keyword / shape
   (position  :initform nil :initarg :position)   ; point
   (pivot     :initform :CC :initarg :pivot)      ; keyword
   (debug     :initform nil :initarg :debug)))    ; (or nil t keyword)


(defmethod initialize-instance :after ((ent use) &rest initargs)
  (declare (ignore initargs))
  (with-slots (pivot debug) ent
    (setf pivot  (or pivot :CC))
    (when debug
      (setf debug (if (keywordp debug) debug :red))))
  ent)

(defmethod check ((ent use) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (ref position pivot layer debug) ent
    (let ((obj (dict-get-entity dict ref)))
      (if (and obj (typep obj 'kaavio:group-definition))
          (setf ref obj)
          (throw-exception "ID '~A' is not found in dictionary or not defgroup object." ref)))
    (setf position (canvas-fix-point canvas position))
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-layer*)))
    (check-member pivot  :nullable nil :types keyword)
    (check-member debug  :nullable t :types keyword))
  nil)

(defmethod attribute-width ((obj use))
  (slot-value (slot-value obj 'ref) 'width))

(defmethod attribute-height ((obj use))
  (slot-value (slot-value obj 'ref) 'height))

(defmethod attribute-center ((obj use))
  (with-slots (position pivot) obj
    (shape-calc-center-using-pivot position pivot
                                   (attribute-width  obj)
                                   (attribute-height obj))))

;;MEMO : use impelementation of shape...
;;(defmethod shape-connect-point ((obj use) type1 type2 arg) ...)
  
;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((obj use)) ...)

(defmethod draw-entity ((obj use) writer)
  (with-slots (ref debug) obj
    (let ((center (attribute-center obj)))
      (pre-draw obj writer)
      (with-slots (width height) ref
        (writer-write writer
                      "<use xlink:href='#" (slot-value ref 'id) "' "
                      "x='" (- (point-x center) (/ width  2)) "' "
                      "y='" (- (point-y center) (/ height 2)) "' />")
        (when debug
          (writer-write writer
                        "<rect "
                        "x='" (- (point-x center) (/ width 2)) "' "
                        "y='" (- (point-y center) (/ height 2)) "' "
                        "width='" width "' "
                        "height='" height "' "
                        "fill='none' "
                        (to-property-strings (make-stroke :color debug :dasharray '(1 4)))
                        "/>")))
      (post-draw obj writer)))
  nil)

;;------------------------------------------------------------------------------
;;
;; macro use
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#              :use
 |#
(defmacro use (ref position &key pivot link rotate layer id contents debug)
  (let ((code `(register-entity (make-instance 'kaavio:use
                                               :ref   ,ref
                                               :position ,position :pivot ,pivot
                                               :link  ,link  :rotate ,rotate
                                               :layer ,layer :id ,id :debug ,debug))))
    (if (null contents)
        code
        (let ((g-obj (gensym "OBJ")))
          `(let* ((,g-obj ,code)
                  (canvas (kaavio:shape-get-subcanvas ,g-obj)))
             (declare (special canvas))
             ,@contents)))))

