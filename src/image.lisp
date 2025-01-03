#|
#|ASD|#             (:file "image"                     :depends-on ("kaavio"
#|ASD|#                                                             "binutil"
#|ASD|#                                                             "shape"
#|ASD|#                                                             "label-info"
#|ASD|#                                                             "link-info"
#|ASD|#                                                             "point"
#|ASD|#                                                             "filter"
#|ASD|#                                                             "writer"))
#|EXPORT|#              ;image.lisp
 |#


(in-package :kaavio)

;;------------------------------------------------------------------------------
;;
;; internal uutilities
;;
;;------------------------------------------------------------------------------
(defun __get-size-of-imagefile (file-name)
  (let ((img-type (string-downcase (pathname-type file-name))))
    (cond
      ((string= img-type "png") ;----------------------------------------
       (bin/with-read-stream (reader file-name)
         (bin/seek-relative reader 16)
         (values (bin/read-value :uint32 reader :endian :big)
                 (bin/read-value :uint32 reader :endian :big))))

      ((or (string= img-type "jpg") ;------------------------------------
           (string= img-type "jpeg"))
       (bin/with-read-stream (reader file-name)
         (bin/seek-relative reader 2) ; skip SOI mark
         (let ((mark (bin/read-value :uint16 reader :endian :big))
               (len  (bin/read-value :uint16 reader :endian :big)))
           (do ()
               ((= mark #xFFC0) nil)
             (bin/seek-relative reader (- len 2))
             (setf mark (bin/read-value :uint16 reader :endian :big))
             (setf len  (bin/read-value :uint16 reader :endian :big)))
           (bin/seek-relative reader 1)
           (let ((height (bin/read-value :uint16 reader :endian :big))
                 (width  (bin/read-value :uint16 reader :endian :big)))
             (values width height)))))

      ((string= img-type "gif") ;----------------------------------------
       (bin/with-read-stream (reader file-name)
         (bin/seek-relative reader 6)
         (values (bin/read-value :uint16 reader :endian :little)
                 (bin/read-value :uint16 reader :endian :little))))

      ((string= img-type "bmp") ;----------------------------------------
       (bin/with-read-stream (reader file-name)
         ;; skip BITMAPFILEHEADER.
         (bin/seek-relative reader 14)
         ;; load bcSize ( 40 : windows bitmap, 12 : OS/2 bitmap )
         (let ((bc-size (bin/read-value :uint32 reader :endian :little)))
           (if (= bc-size 40)
               (values (bin/read-value :uint32 reader :endian :little)
                       (bin/read-value :uint32 reader :endian :little))
               (if (= bc-size 12)
                   (values (bin/read-value :uint16 reader :endian :little)
                           (bin/read-value :uint16 reader :endian :little)))))))

      (t (values nil nil)))))
       


;;------------------------------------------------------------------------------
;;
;; class image
;;
;;------------------------------------------------------------------------------
(defclass image (shape)
  ((filename        :initform nil :initarg :filename)    ; (or string pathname)
   (width           :initform   0 :initarg :width)       ; number
   (height          :initform   0 :initarg :height)      ; number
   (position        :initform nil :initarg :position)    ; point
   (pivot           :initform :CC :initarg :pivot)       ; keyword
   (label           :initform nil :initarg :label)       ; (or nil label-info)
   (preserve-ratio  :initform nil)                       ; boolean
   (filter          :initform nil :initarg :filter)))    ; (or nil keyword)


(defmethod initialize-instance :after ((img image) &rest initargs)
  (declare (ignore initargs))
  (with-slots (pivot label filter layer) img
    (setf pivot  (or pivot :CC))
    (when label
      (setf label (make-label label)))
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-filter*)))
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-layer*))))
  img)


(defmethod check ((img image) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (position pivot filename width height label preserve-ratio filter) img
    (check-member filename :nullable nil :types (or pathname string))
    (check-member width    :nullable   t :types number)
    (check-member height   :nullable   t :types number)
    (check-member pivot    :nullable nil :types keyword)
    (check-object label    canvas dict :nullable t :class label-info)
    (check-member filter   :nullable   t :types keyword)
    (let ((path (merge-pathnames filename (path/get-current-directory))))
      ;; 指定された名前のファイルがカレントディレクトリに存在することをチェック
      (unless (path/is-existing-file path)
        (throw-exception "image file '~A' is not exist." path))
      ;; width, height の両方とも明示的に指定されている場合、アスペクト比の維持をしない
      (if (and width height)
          (setf preserve-ratio nil)
          ;; 上記以外の場合、画像ファイルの実際のサイズを取得する。
          (multiple-value-bind (w h) (__get-size-of-imagefile path)
            ;; width, height の両方とも nil ならば実際のサイズを設定
            (if (and (null width) (null height))
                (setf width  w
                      height h)
                ;; 上記以外の場合、実サイズのアスペクト比にあわせて nil 側を設定
                (if (null width)
                    (setf width  (* height (/ w h)))
                    (setf height (* width  (/ h w))))))))
    (check-member width  :nullable nil :types number)
    (check-member height :nullable nil :types number)
    (setf position (canvas-fix-point canvas position)))
  nil)


(defmethod attribute-width ((img image))
  (slot-value img 'width))

(defmethod attribute-height ((img image))
  (slot-value img 'height))

(defmethod attribute-center ((img image))
  (with-slots (position pivot width height) img
    (shape-calc-center-using-pivot position pivot width height)))

;;MEMO : use impelementation of shape...
;;(defmethod shape-connect-point ((img image) type1 type2 arg) ...)
  
;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((img image)) ...)

(defmethod entity-composition-p ((img image))
  (if (slot-value img 'label)
      t
      (call-next-method)))

(defmethod draw-entity ((img image) writer)
  (with-slots (width height filename preserve-ratio filter) img
    (let ((id (and (not (entity-composition-p img))
                   (slot-value img 'id)))
          (topleft (attribute-topleft img)))
      (pre-draw img writer)
      (writer-write writer
                    "<image "
                    (write-when (keywordp id) "id='" id "' ")
                    "x='" (point-x topleft) "' "
                    "y='" (point-y topleft) "' "
                    "width='"  width  "' "
                    "height='" height "' "
                    "xlink:href='" filename "' "
                    (unless preserve-ratio
                      "preserveAspectRatio='none' ")
                    (write-when filter "filter='url(#" it ")' ")
                    "/>")
      (with-slots (label) img
        (when label
          (draw-label label img writer)))
      (post-draw img writer)))
  nil)


;;------------------------------------------------------------------------------
;;
;; macro image
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#              :image
 |#
(defmacro image (position filename
                 &key pivot width height label link rotate layer id filter contents)
  (let ((code `(register-entity (make-instance 'kaavio:image
                                               :filename ,filename
                                               :width ,width :height ,height
                                               :position ,position :pivot ,pivot
                                               :label ,label
                                               :link ,link :rotate ,rotate
                                               :filter ,filter :layer ,layer :id ,id))))
    (if (null contents)
        code
        (let ((g-obj (gensym "OBJ")))
          `(let* ((,g-obj ,code)
                  (canvas (kaavio:shape-get-subcanvas ,g-obj)))
             (declare (special canvas))
             ,@contents)))))

