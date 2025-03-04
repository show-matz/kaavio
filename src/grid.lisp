#|
#|ASD|#                (:file "grid"                      :depends-on ("kaavio"
#|ASD|#                                                                "colormap"
#|ASD|#                                                                "stroke-info"
#|ASD|#                                                                "entity"
#|ASD|#                                                                "writer"))
#|EXPORT|#                ;grid.lisp
 |#

(in-package :kaavio)

;;------------------------------------------------------------------------------
;;
;; class grid
;;
;;------------------------------------------------------------------------------
(defclass grid (entity)
  ((size    :initform nil  :initarg  :size)        ; number
   (bgcolor :initform nil  :initarg  :bgcolor)     ; (or keyword string)
   (stroke  :initform nil  :initarg  :stroke)))    ; (or nil stroke-info)
  

(defmethod initialize-instance :after ((grd grid) &rest initargs)
  (declare (ignore initargs))
  (with-slots (size bgcolor stroke layer) grd
    (setf size    (or size 10))
    (setf bgcolor (or bgcolor :white))
    (setf stroke  (make-stroke (or stroke '(:color :gray :width 0.2))))
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-layer*))))
  grd)

(defmethod check ((grd grid) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (size bgcolor stroke) grd
    (check-member size    :nullable nil :types number)
    (check-member bgcolor :nullable nil :types (or string keyword))
    (setf bgcolor (colormap-fix bgcolor))
    (check-object stroke  canvas dict :nullable  nil :class stroke-info))
  nil)

(defmethod draw-entity ((grd grid) writer)
  (with-slots (size bgcolor stroke) grd
    (writer-write writer
                  "<pattern "
                      "id='__grid' "
                      "width='" size "' "
                      "height='" size "' "
                      "patternUnits='userSpaceOnUse' "
                      ">")
    (writer-incr-level writer)
    (writer-write writer
                  "<rect "
                      "x='0' y='0' "
                      "width='" size "' "
                      "height='" size "' "
                      "fill='" bgcolor "' "
                  "/>")
    (writer-write writer
                  "<polyline "
                      "fill='none' "
                      (to-property-strings stroke)
                      "points='" size ",0 0,0 0," size "' "
                  "/>")
    (writer-decr-level writer)
    (writer-write writer "</pattern>")
    (writer-write writer
                  "<rect "
                      "x='0' y='0' "
                      "width='100%' "
                      "height='100%' "
                      "fill='url(#__grid)' "
                      (to-property-strings stroke)
                  "/>"))
  nil)

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro grid
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{grid}} ${KEY} (size 10) (bgcolor :white) stroke layer
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `size` ---- 挿入する SVG コード片を文字列で指定します。
;;* `bgcolor` ---- 背景の塗り潰しを指定します。
;;* `stroke` ----  グリッド線を描画するストロークを指定します。
;;* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
;;
;;${DESCRIPTION}
;;
;;　グリッド線を描画します。現状では常に SVG 画像全体を埋める矩形のパターンとなります。
;;（サブキャンバスなど）特定の領域だけにグリッド線を描画することはできません。
;;
;;${NO_SEE_ALSO}
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :grid
 |#
(defmacro grid (&key (size 10) (bgcolor :white) stroke layer)
  `(register-entity (make-instance 'grid
                                   :size ,size
                                   :bgcolor ,bgcolor
                                   :stroke ,stroke :layer ,layer)))

