#|
#|ASD|#                (:file "regular-polygon"           :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "mathutil"
#|ASD|#                                                                "canvas"
#|ASD|#                                                                "point"
#|ASD|#                                                                "circle"
#|ASD|#                                                                "stroke-info"
#|ASD|#                                                                "link-info"
#|ASD|#                                                                "clipping"
#|ASD|#                                                                "filter"
#|ASD|#                                                                "writer"))
#|EXPORT|#                ;regular-polygon.lisp
 |#


(in-package :kaavio)

;; 「正Ｎ角形」における N 個の点をリストにして返す関数
;;    pt ---- 中心点の座標
;;    radius ---- ベースとなる正円の半径
;;    count ---- 正Ｎ角形の N
(defun regular-polygon-listup-points (pt radius count tilt)
  (let ((delta (ecase count
                 (( 3) (/ 360  3))
                 (( 4) (/ 360  4))
                 (( 5) (/ 360  5))
                 (( 6) (/ 360  6))
                 (( 8) (/ 360  8))
                 ((10) (/ 360 10))
                 ((12) (/ 360 12)))))
    (labels ((recur (idx acc)
               (if (= idx count)
                   (nreverse acc)
                   (let ((degree (mod (+ 360 -90 (* idx delta) tilt) 360)))
                     (recur (1+ idx)
                            (push (xy+ pt (* radius (math/cos1 degree))
                                       (* radius (math/sin1 degree))) acc))))))
      (recur 0 nil))))

(defun regular-polygon-apply-offsets (points offsets &optional acc)
  (if (null points)
      (nreverse acc)
      (regular-polygon-apply-offsets (cdr points) (cdr offsets)
                                     (push (point+ (car points)
                                                   (or (car offsets) '(0 0))) acc))))


;;------------------------------------------------------------------------------
;;
;; class regular-polygon
;;
;;------------------------------------------------------------------------------
(defclass regular-polygon (circle)
  ((count   :initform nil :initarg :count)      ; number - must be 3, 4, 5, 6, 8,10 or 12.
   (tilt    :initform nil :initarg :tilt)       ; number
   (offsets :initform nil :initarg :offsets)    ; list of point
   (debug   :initform nil :initarg :debug)))    ; (or nil t keyword)

(defmethod initialize-instance :after ((ent regular-polygon) &rest initargs)
  (declare (ignore initargs))
  (with-slots (pivot fill stroke filter debug) ent
    (setf pivot  (or pivot :CC))
    (setf fill   (make-fill   (or fill   *default-fill*   :none)))
    (setf stroke (make-stroke (or stroke *default-stroke* :none)))
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-filter*)))
    (when debug
      (setf debug (if (keywordp debug) debug :red))))
  ent)

(defmethod check ((shp regular-polygon) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (count tilt offsets debug) shp
    (check-numbers count 3 4 5 6 8 10 12)
    (check-member  offsets :nullable t :types list)
    (check-member  debug   :nullable t :types keyword)
    (labels ((chk-offsets (lst)
               (unless (null lst)
                 (let ((pt (car lst)))
                   (unless (point-p pt)
                     (throw-exception "Invalid point '~A' in points of line." pt))
                   (chk-offsets (cdr lst))))))
      (chk-offsets offsets)))
  nil)

(defmethod attribute-width ((shp regular-polygon))
  (* 2 (slot-value shp 'radius)))

(defmethod attribute-height ((shp regular-polygon))
  (* 2 (slot-value shp 'radius)))

(defmethod attribute-center ((shp regular-polygon))
  (with-slots (position pivot radius) shp
    (shape-calc-center-using-pivot position pivot (* 2 radius) (* 2 radius))))


(defmethod shape-connect-point ((shp regular-polygon) type1 type2 arg)
  (circle-connect-point (attribute-center shp)
                        (slot-value shp 'radius) type1 type2 arg))

;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((shp regular-polygon)) ...)

;;MEMO : use impelementation of shape...
;;(defmethod entity-composition-p ((shp regular-polygon)) ...)

(defmethod draw-entity ((shp regular-polygon) writer)
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
    (with-slots (count tilt offsets radius fill stroke clip-path filter debug) shp
      (let* ((id (and (not (entity-composition-p shp))
                      (slot-value shp 'id)))
             (center (attribute-center shp))
             (points (regular-polygon-apply-offsets
                      (regular-polygon-listup-points center radius count tilt) offsets)))
        (pre-draw shp writer)
        (when debug
          (writer-write writer
                        "<circle "
                        "cx='" (point-x center) "' "
                        "cy='" (point-y center) "' "
                        "r='" radius "' "
                        "fill='none' "
                        (to-property-strings (make-stroke :color debug :dasharray '(2 4)))
                        "/>"))
        (writer-write writer
                      "<polygon "
                      (write-when (keywordp id) "id='" id "' ")
                      (to-property-strings fill)
                      (to-property-strings stroke)
                      "points='" (format-points points) "' "
                      (write-when clip-path "clip-path='url(#" it ")' ")
                      (write-when filter "filter='url(#" it ")' ")
                      "/>")
        (when debug
          (writer-write writer
                        "<circle "
                        "cx='" (coerce (point-x (car points)) 'single-float) "' "
                        "cy='" (coerce (point-y (car points)) 'single-float) "' "
                        "r='3' "
                        "stroke='none' "
                        (to-property-strings (make-fill :color debug))
                        "/>"))
      (post-draw shp writer))))
  nil)


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro regular-polygon
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{regular-polygon}} position n size ${KEY} pivot tilt offsets rotate fill stroke link layer id filter debug contents
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `n` ---- 正Ｎ角形を描く場合の N を指定します。現在、3 4 5 6 8 10 12 が使用できます。
;;* `size` ---- ベースとなる正円の半径を数値で指定します。
;;* `pivot` ---- 基準点がベースとなる正円のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `tilt` ----  多角形の傾きを角度で指定します。 `rotate` とは異なり、`offsets` 適用前に回転されます。
;;* `offsets` ---- 各点を移動させるためのオフセットのリストを指定します。詳細は後述します。
;;* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
;;* `fill` ---- 内部の塗り潰しを指定します。
;;* `stroke` ---- 円を描画するストロークを指定します。
;;* `link` ---- リンクにする場合、リンク先を指定します。
;;* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
;;* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。
;;* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
;;* `debug` ---- 補助線を描画する場合、 `t` または色名を指定します
;;* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。
;;
;;${DESCRIPTION}
;;
;;　正多角形を描画します。複数の基本要素でスタイルを統一したい場合、with-options マクロを
;;使うことができます。
;;
;;　`offsets` パラメータを使用することで、「正」でない多角形を描画することができます。たとえば
;;三角形に対して `:offsets '((0 0) (-10 0) (10 0))` とすれば二等辺三角形を描画できます。
;;`offsets` は点のリストで、それぞれの点は多角形の頂点に順に座標を移動させるオフセットとして
;;適用されます。ここで最初に適用される点は、常に中央一番上の点であり、そこから時計回りに順番と
;;なります。
;;
;;${SEE_ALSO}
;;
;;* 正多角形
;;* 多角形
;;
;;${NOTES}
;;
;;　regular-polygon マクロが生成する正多角形への接続点は、 `size` を半径とする正円の上に
;;配置されます。
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :regular-polygon
 |#
(defmacro regular-polygon (position n size
                           &key pivot tilt offsets rotate fill stroke link layer id filter debug contents)
  (let ((code `(register-entity (make-instance 'kaavio:regular-polygon
                                               :count ,n :position ,position :pivot ,pivot
                                               :tilt ,(or tilt 0) :offsets ,offsets :rotate ,rotate
                                               :radius ,size :fill ,fill :stroke ,stroke
                                               :clip-path *current-clip-path* :link ,link
                                               :filter ,filter :layer ,layer :id ,id :debug ,debug))))
    (if (null contents)
        code
        (let ((g-obj (gensym "OBJ")))
          `(let* ((,g-obj ,code)
                  (canvas (kaavio:shape-get-subcanvas ,g-obj)))
             (declare (special canvas))
             ,@contents)))))

