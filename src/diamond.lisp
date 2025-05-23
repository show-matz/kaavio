#|
#|ASD|#                (:file "diamond"                   :depends-on ("kaavio"
#|ASD|#                                                                "mathutil"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "canvas"
#|ASD|#                                                                "point"
#|ASD|#                                                                "shape"
#|ASD|#                                                                "stroke-info"
#|ASD|#                                                                "link-info"
#|ASD|#                                                                "clipping"
#|ASD|#                                                                "filter"
#|ASD|#                                                                "writer"))
#|EXPORT|#                ;diamond.lisp
 |#


(in-package :kaavio)

;;------------------------------------------------------------------------------
;;
;; utility functions
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#                :diamond-connect-point
 |#
(defun diamond-connect-point-C (cx cy w h pt)
  (let ((pt (xy+ pt (- cx) (- cy))))
    (with-point (px py) pt
      (let ((ret (cond
                   ((and (<= 0 px) (<= 0 py))
                    (math/intersection-point '(0 0) pt `(0 ,(+ (/ h 2))) `(,(+ (/ w 2)) 0)))
                   ((and (<= 0 px) (< py  0))
                    (math/intersection-point '(0 0) pt `(0 ,(- (/ h 2))) `(,(+ (/ w 2)) 0)))
                   ((and (< px  0) (<= 0 py))
                    (math/intersection-point '(0 0) pt `(0 ,(+ (/ h 2))) `(,(- (/ w 2)) 0)))
                   ((and (< px  0) (< py  0))
                    (math/intersection-point '(0 0) pt `(0 ,(- (/ h 2))) `(,(- (/ w 2)) 0))))))
        (make-point (+ (point-x ret) cx)
                    (+ (point-y ret) cy)  :absolute)))))

(defun diamond-connect-point-T (cx cy w h idx)
  (let* ((magic (if (zerop idx) 0 1)))
    (make-point (+ cx (* (/ w 6) idx))
                (+ cy (- (/ h 2)) (* (/ h 6) magic)) :absolute)))

(defun diamond-connect-point-B (cx cy w h idx)
  (let* ((magic (if (zerop idx) 0 1)))
    (make-point (+ cx (* (/ w 6) idx))
                (- cy (- (/ h 2)) (* (/ h 6) magic)) :absolute)))

(defun diamond-connect-point-L (cx cy w h idx)
  (let* ((magic (if (zerop idx) 0 1)))
    (make-point (+ cx (- (/ w 2)) (* (/ w 6) magic))
                (+ cy (* (/ h 6) idx)) :absolute)))

(defun diamond-connect-point-R (cx cy w h idx)
  (let* ((magic (if (zerop idx) 0 1)))
    (make-point (- cx (- (/ w 2)) (* (/ w 6) magic))
                (+ cy (* (/ h 6) idx)) :absolute)))


(defun diamond-connect-point (center w h type1 type2 arg)
  (declare (ignore type1))
  (let ((cx (point-x center))
        (cy (point-y center))
        (handler (ecase type2
                   ((:center) #'diamond-connect-point-C)
                   ((:top)    #'diamond-connect-point-T)
                   ((:bottom) #'diamond-connect-point-B)
                   ((:left)   #'diamond-connect-point-L)
                   ((:right)  #'diamond-connect-point-R))))
    (funcall handler cx cy w h arg)))


;;-------------------------------------------------------------------------------
;;
;; class diamond
;;
;;-------------------------------------------------------------------------------
(defclass diamond (shape)
  ((position  :initform nil :initarg :position)    ; point
   (pivot     :initform :CC :initarg :pivot)       ; keyword
   (width     :initform   0 :initarg :width)       ; number
   (height    :initform   0 :initarg :height)      ; number
   (fill      :initform nil :initarg :fill)        ; (or nil fill-info)
   (stroke    :initform nil :initarg :stroke)      ; (or nil fill-info)
   (clip-path :initform nil :initarg :clip-path)   ; (or nil symbol)
   (filter    :initform nil :initarg :filter)))    ; (or nil keyword)


(defmethod initialize-instance :after ((rct diamond) &rest initargs)
  (declare (ignore initargs))
  (with-slots (pivot fill stroke filter layer) rct
    (setf pivot  (or pivot :CC))
    (setf fill   (make-fill   (or fill   *default-fill*   :none)))
    (setf stroke (make-stroke (or stroke *default-stroke* :none)))
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-filter*)))
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-layer*))))
  rct)

(defmethod check ((rct diamond) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (position pivot width height fill stroke clip-path filter) rct
    (check-member pivot     :nullable nil :types keyword)
    (check-member width     :nullable nil :types number)
    (check-member height    :nullable nil :types number)
    (check-object fill      canvas dict :nullable nil :class   fill-info)
    (check-object stroke    canvas dict :nullable nil :class stroke-info)
    (check-member clip-path :nullable   t :types symbol)
    (check-member filter    :nullable   t :types keyword)
    (setf position (canvas-fix-point canvas position)))
  nil)

(defmethod attribute-width ((rct diamond))
  (slot-value rct 'width))

(defmethod attribute-height ((rct diamond))
  (slot-value rct 'height))

(defmethod attribute-center ((rct diamond))
  (with-slots (position pivot width height) rct
    (shape-calc-center-using-pivot position pivot width height)))

(defmethod shape-connect-point ((shp diamond) type1 type2 arg)
  (diamond-connect-point (attribute-center   shp)
                         (slot-value shp 'width)
                         (slot-value shp 'height) type1 type2 arg))
  
;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((shp diamond)) ...)

(defmethod draw-entity ((rct diamond) writer)
  (labels ((format-points (pts)
             (with-output-to-string (stream)
               (do ((idx 0 (incf idx)))
                   ((null pts) nil)
                 (unless (zerop idx)
                   (princ #\space stream))
                 (format stream "~A,~A"
                         (coerce (point-x (car pts)) 'single-float)
                         (coerce (point-y (car pts)) 'single-float))
                 (setf pts (cdr pts))))))
    (with-slots (width height fill stroke clip-path filter) rct
      (let* ((center (attribute-center rct))
             (id (and (not (entity-composition-p rct))
                      (slot-value rct 'id)))
             ;(topleft (attribute-topleft rct))
             (points  (list (y+ center (- (/ height 2)))
                            (x+ center (- (/ width  2)))
                            (y+ center (+ (/ height 2)))
                            (x+ center (+ (/ width  2)))
                            (y+ center (- (/ height 2))))))
        (pre-draw rct writer)
        (writer-write writer
                      "<polygon "
                      (write-when (keywordp id) "id='" id "' ")
                      (to-property-strings fill)
                      (to-property-strings stroke)
                      "points='" (format-points points) "' "
                      (write-when clip-path "clip-path='url(#" it ")' ")
                      (write-when filter "filter='url(#" it ")' ")
                      "/>")
        (post-draw rct writer))))
  nil)
  

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro diamond
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{diamond}} position width height ${KEY} pivot fill stroke rotate link layer id filter contents
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `width` ---- 幅を数値で指定します。
;;* `height` ---- 高さを数値で指定します。
;;* `pivot` ---- 基準点がひし形のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `fill` ---- 内部の塗り潰しを指定します。
;;* `stroke` ---- 外枠を描画するストロークを指定します。
;;* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
;;* `link` ---- リンクにする場合、リンク先を指定します。
;;* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
;;* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。
;;* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
;;* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。
;;
;;${DESCRIPTION}
;;
;;　ひし形を描画します。複数の基本要素でスタイルを統一したい場合、with-options マクロを
;;使うことができます。
;;
;;${SEE_ALSO}
;;
;;* ひし形
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :diamond
 |#
(defmacro diamond (position width height
                     &key pivot fill stroke rotate link layer id filter contents)
  (let ((code `(register-entity (make-instance 'kaavio:diamond
                                               :position ,position :pivot ,pivot
                                               :width ,width :height ,height
                                               :fill ,fill :stroke ,stroke
                                               :rotate ,rotate :link ,link
                                               :clip-path *current-clip-path*
                                               :filter ,filter :layer ,layer :id ,id))))
    (if (null contents)
        code
        (let ((g-obj (gensym "OBJ")))
          `(let* ((,g-obj ,code)
                  (canvas (kaavio:shape-get-subcanvas ,g-obj)))
             (declare (special canvas))
             ,@contents)))))

