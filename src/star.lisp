#|
#|ASD|#                (:file "star"                      :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "path"
#|ASD|#                                                                "filter"
#|ASD|#                                                                "text-shape"))
#|EXPORT|#                ;star.lisp
 |#

(in-package :kaavio)

#|
#|EXPORT|#                :*default-star-font*
#|EXPORT|#                :*default-star-fill*
#|EXPORT|#                :*default-star-stroke*
#|EXPORT|#                :*default-star-filter*
#|EXPORT|#                :*default-star-layer*
 |#
(defparameter *default-star-font*         nil)
(defparameter *default-star-fill*         nil)
(defparameter *default-star-stroke*       nil)
(defparameter *default-star-filter*       nil)
(defparameter *default-star-layer*        nil)

(defun star-get-points (points center w1 h1 ratio tilt)
  (let* ((pts    nil)
         (ratio (or ratio
                    (if (<= 10 points 24)
                        0.850
                        (ecase points
                          ((3) 0.100)
                          ((4) 0.200)
                          ((5) 0.380)
                          ((6) 0.580)
                          ((7) 0.695)
                          ((8) 0.770)
                          ((9) 0.816)))))
         (delta (/ 360 points))
         (w2    (* w1 ratio))
         (h2    (* h1 ratio)))
    (dotimes (idx points)
      (let ((degree (mod (+ (* idx delta) tilt 270) 360)))
        (push (xy+ center
                   (* (/ w1 2) (kaavio::math/cos1 degree))
                   (* (/ h1 2) (kaavio::math/sin1 degree))) pts))
      (let ((degree (mod (+ (* idx delta) tilt 270 (/ delta 2)) 360)))
        (push (xy+ center
                   (* (/ w2 2) (kaavio::math/cos1 degree))
                   (* (/ h2 2) (kaavio::math/sin1 degree))) pts)))
    pts))

;;------------------------------------------------------------------------------
;;
;; class star
;;
;;------------------------------------------------------------------------------
(defclass star (text-shape)
  ((points :initform nil :initarg :points)   ; number - must be 3 ~ 24.
   (ratio  :initform nil :initarg :ratio)    ; number - 0.1 ~ 0.85
   (tilt   :initform nil :initarg :tilt)     ; number
   (filter :initform nil :initarg :filter))) ; (or nil keyword)

(defmethod initialize-instance :after ((obj star) &rest initargs)
  (declare (ignore initargs))
  (with-slots (filter layer) obj
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-star-filter* *default-filter*)))
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-star-layer* *default-layer*))))
  obj)

(defmethod check ((obj star) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (points filter height) obj
    (check-numbers points 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24)
    (check-member  filter :nullable   t :types keyword))
  nil)

;; override of group::draw-group
(defmethod draw-group ((obj star) writer)
  (let* ((canvas (group-get-canvas obj))
         (center (canvas-center  canvas))
         (width  (canvas-width  canvas))
         (height (canvas-height canvas)))
    (macrolet ((register-entity (entity)
                 `(check-and-draw-local-entity ,entity canvas writer)))
      (with-slots (points ratio tilt fill stroke filter clip-path) obj
        ;; draw
        (let ((*current-clip-path* clip-path))
          (polygon (star-get-points points center width height ratio tilt)
                                    :stroke stroke :fill fill :filter filter)))))
  ;; draw text
  (call-next-method))

;; no override.
;(defmethod text-shape-calc-size ((obj star))
;  (call-next-method))

;; no override.
;(defmethod text-shape-paragraph-area ((obj star))
;  (call-next-method))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro star
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{star}} position n width height text ${KEY} pivot font fill stroke link rotate layer id filter contents
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `n` ----  頂点の数を指定します。現在、3 以上 24 以下が使用できます。
;;* `width` ----  幅を数値で指定します。
;;* `height` ----  高さを数値で指定します。
;;* `text` ----  内部に描画するテキストを文字列で指定します。改行は "~%" で表現します。
;;* `tilt` ----  星型の傾きを角度で指定します。 `rotate` とは異なり、テキストは回転しません。
;;* `ratio` ----  頂点の鋭さを指定します。0.00 より大きく 1.00 より小さい数値を指定します。詳細は後述します。
;;* `pivot` ----  基準点が星型のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `font` ----  フォントを指定します。
;;* `fill` ----  内部の塗り潰しを指定します。
;;* `stroke` ----  外枠を描画する線を指定します。
;;* `link` ----  リンクにする場合、リンク先を指定します。
;;* `rotate` ----  回転させたい場合に、その角度を指定します。
;;* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
;;* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
;;* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
;;* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。
;;
;;${DESCRIPTION}
;;
;;　星型を描画します。複数の星型でスタイルを統一したい場合、with-star-options マクロを
;;使うことができます。
;;
;;${SEE_ALSO}
;;
;;* 星型
;;* with-star-options マクロ
;;
;;${NOTES}
;;
;;　`ratio` について説明します。これは「凹んでいる頂点の中心からの距離を、尖っている部分の
;;中心からの距離に対する比率で表したもの」です。省略した場合のデフォルト値は n の値に応じて
;;以下のように変化します。
;;
;;| n   | default value |
;;|:---:|:-------------:|
;;| 3   | 0.100         |
;;| 4   | 0.200         |
;;| 5   | 0.380         |
;;| 6   | 0.580         |
;;| 7   | 0.695         |
;;| 8   | 0.770         |
;;| 9   | 0.816         |
;;| 10+ | 0.850         |
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :star
 |#
(defmacro star (position n width height text
                         &key tilt ratio pivot font fill stroke link rotate layer id filter contents)
  (let ((code `(register-entity (make-instance 'star
                                               :position ,position :pivot ,pivot
                                               :points ,n :text ,text
                                               :tilt ,(or tilt 0) :ratio ,ratio
                                               :width ,width :height ,height
                                               :font   (or ,font   *default-star-font*)
                                               :fill   (or ,fill   *default-star-fill*)
                                               :stroke (or ,stroke *default-star-stroke*)
                                               :link ,link  :rotate ,rotate
                                               :clip-path *current-clip-path*
                                               :filter ,filter :layer ,layer :id ,id))))
    (if (null contents)
        code
        (let ((g-obj (gensym "OBJ")))
          `(let* ((,g-obj ,code)
                  (canvas (kaavio:shape-get-subcanvas ,g-obj)))
             (declare (special canvas))
             ,@contents)))))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-star-options
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-star-options}} (${KEY} font fill stroke filter layer) ${BODY} body
;;
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　star マクロで描画される星型のデフォルトオプションを変更します。キーワードパラメータ
;;群の説明は star マクロを参照してください。
;;
;;${SEE_ALSO}
;;
;;* 星型
;;* star マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-star-options
 |#
(defmacro with-star-options ((&key (font   nil font-p)
                                   (fill   nil fill-p)
                                   (stroke nil stroke-p)
                                   (filter nil filter-p)
                                   (layer  nil layer-p)) &rest body)
  (let ((bindings nil))
    (labels ((impl (arg-p binding)
               (when arg-p (push binding bindings))))
      (impl font-p   `(*default-star-font*   (make-font2   *default-star-font*   ,font)))
      (impl fill-p   `(*default-star-fill*   (make-fill2   *default-star-fill*   ,fill)))
      (impl stroke-p `(*default-star-stroke* (make-stroke2 *default-star-stroke* ,stroke)))
      (impl filter-p `(*default-star-filter* ,filter))
      (impl layer-p  `(*default-star-layer*  ,layer)))
    `(let ,(nreverse bindings)
       ,@body)))
