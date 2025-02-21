#|
#|ASD|#                (:file "regular-polygon"           :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "mathutil"
#|ASD|#                                                                "canvas"
#|ASD|#                                                                "point"
#|ASD|#                                                                "circle"
#|ASD|#                                                                "stroke-info"
#|ASD|#                                                                "link-info"
#|ASD|#                                                                "filter"
#|ASD|#                                                                "writer"))
#|EXPORT|#                ;regular-polygon.lisp
 |#


(in-package :kaavio)

;; 「正Ｎ角形」における N 個の点をリストにして返す関数
;;    pt ---- 中心点の座標
;;    radius ---- ベースとなる正円の半径
;;    count ---- 正Ｎ角形の N
(defun regular-polygon-listup-points (pt radius count)
  (let ((delta (ecase count    ;;ToDo : regular-polygon::check で値チェックしてるのでここでは不要かな
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
                   (let ((degree (mod (+ 360 -90 (* idx delta)) 360)))
                     (recur (1+ idx)
                            (push (xy+ pt (* radius (math/cos1 degree))
                                       (* radius (math/sin1 degree))) acc))))))
      (recur 0 nil))))


;;------------------------------------------------------------------------------
;;
;; class regular-polygon
;;
;;------------------------------------------------------------------------------
(defclass regular-polygon (circle)
  ((count :initform nil :initarg :count)))  ; number - must be 3, 4, 5, 6, 8,10 or 12.


(defmethod initialize-instance :after ((ent regular-polygon) &rest initargs)
  (declare (ignore initargs))
  (with-slots (pivot fill stroke filter) ent
    (setf pivot  (or pivot :CC))
    (setf fill   (make-fill   (or fill   *default-fill*   :none)))
    (setf stroke (make-stroke (or stroke *default-stroke* :none)))
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-filter*))))
  ent)

(defmethod check ((shp regular-polygon) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (count) shp
    (check-numbers count 3 4 5 6 8 10 12))
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

;;ToDo : implement...  
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
    (with-slots (count radius fill stroke filter) shp
      (let* ((id (and (not (entity-composition-p shp))
                      (slot-value shp 'id)))
             (center (attribute-center shp))
             (points (regular-polygon-listup-points center radius count)))
        (pre-draw shp writer)
;;      (writer-write writer
;;                    "<circle "
;;                    "cx='" (point-x center) "' "
;;                    "cy='" (point-y center) "' "
;;                    "r='" radius "' "
;;                    "fill='none' "
;;                    (to-property-strings (make-stroke :color :red :dasharray '(1 4)))
;;                    "/>")
        (writer-write writer
                      "<polygon "
                      (write-when (keywordp id) "id='" id "' ")
                      (to-property-strings fill)
                      (to-property-strings stroke)
                      "points='" (format-points points) "' "
                      (write-when filter "filter='url(#" it ")' ")
                      "/>")
      (post-draw shp writer))))
  nil)


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro regular-polygon
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{regular-polygon}} position n size ${KEY} pivot fill stroke link layer id filter contents
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `n` ---- 正Ｎ角形を描く場合の N を指定します。現在、3 4 5 6 8 10 12 が使用できます。
;;* `size` ---- ベースとなる正円の半径を数値で指定します。
;;* `pivot` ---- 基準点が正円のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `fill` ---- 内部の塗り潰しを指定します。
;;* `stroke` ---- 円を描画するストロークを指定します。
;;* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
;;* `link` ---- リンクにする場合、リンク先を指定します。
;;* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
;;* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。
;;* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
;;* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。
;;
;;${DESCRIPTION}
;;
;;　正多角形を描画します。複数の基本要素でスタイルを統一したい場合、with-options マクロを
;;使うことができます。
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
                           &key pivot fill stroke rotate link layer id filter contents)
  (let ((code `(register-entity (make-instance 'kaavio:regular-polygon
                                               :count ,n :position ,position
                                               :pivot ,pivot :radius ,size
                                               :fill ,fill :stroke ,stroke :rotate ,rotate
                                               :link ,link :filter ,filter :layer ,layer :id ,id))))
    (if (null contents)
        code
        (let ((g-obj (gensym "OBJ")))
          `(let* ((,g-obj ,code)
                  (canvas (kaavio:shape-get-subcanvas ,g-obj)))
             (declare (special canvas))
             ,@contents)))))

