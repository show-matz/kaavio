#|
#|ASD|#                (:file "textbox"                   :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "rectangle"
#|ASD|#                                                                "filter"
#|ASD|#                                                                "text-shape"))
#|EXPORT|#                ;textbox.lisp
 |#

(in-package :kaavio)

#|
#|EXPORT|#                :*default-textbox-rx*
#|EXPORT|#                :*default-textbox-ry*
#|EXPORT|#                :*default-textbox-align*
#|EXPORT|#                :*default-textbox-valign*
#|EXPORT|#                :*default-textbox-margin*
#|EXPORT|#                :*default-textbox-font*
#|EXPORT|#                :*default-textbox-fill*
#|EXPORT|#                :*default-textbox-stroke*
#|EXPORT|#                :*default-textbox-filter*
#|EXPORT|#                :*default-textbox-layer*
 |#
(defparameter *default-textbox-rx*           nil)
(defparameter *default-textbox-ry*           nil)
(defparameter *default-textbox-align*        :center)
(defparameter *default-textbox-valign*       :center)
(defparameter *default-textbox-margin*       10)
(defparameter *default-textbox-font*         nil)
(defparameter *default-textbox-fill*         nil)
(defparameter *default-textbox-stroke*       nil)
(defparameter *default-textbox-filter*       nil)
(defparameter *default-textbox-layer*        nil)

;;------------------------------------------------------------------------------
;;
;; class textbox
;;
;;------------------------------------------------------------------------------
(defclass textbox (text-shape)
  ((no-frame  :initform nil :initarg :no-frame)    ; number
   (rx        :initform nil :initarg       :rx)    ; number
   (ry        :initform nil :initarg       :ry)    ; number
   (filter    :initform nil :initarg   :filter)))  ; (or nil keyword)
  
(defmethod initialize-instance :after ((box textbox) &rest initargs)
  (declare (ignore initargs))
  (with-slots (filter layer) box
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-textbox-filter* *default-filter*)))
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-textbox-layer* *default-layer*))))
  box)
   
;; override of group::draw-group
(defmethod draw-group ((box textbox) writer)
  (let* ((canvas (group-get-canvas box))
         (width  (canvas-width  canvas))
         (height (canvas-height canvas)))
    (macrolet ((register-entity (entity)
                 `(check-and-draw-local-entity ,entity canvas writer)))
      (with-slots (no-frame rx ry fill stroke filter clip-path) box
        (unless no-frame
          ;; draw box
          (let ((*current-clip-path* clip-path))
            (rectangle (list (/ width 2) (/ height 2)) width height
                       :rx rx :ry ry :fill fill :stroke stroke :filter filter))))))
  ;; draw text
  (call-next-method))

;; no override.
;(defmethod text-shape-calc-size ((box textbox))
;  (call-next-method))

;; no override.
;(defmethod text-shape-paragraph-area ((box textbox))
;  (call-next-method))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro textbox
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{textbox}} position text ${KEY} pivot width height no-frame rx ry align valign margin font fill stroke link rotate layer id filter contents
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `text` ---- 内部に描画するテキストを文字列で指定します。改行は "~%" で表現します。
;;* `pivot` ---- 基準点がテキストボックスのどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `width` ---- 幅を自動決定せず、明示的に指定したい場合に数値で指定します。
;;* `height` ---- 高さを自動決定せず、明示的に指定したい場合に数値で指定します。
;;* `no-frame` ---- ボックスを描画せず、テキストのみにしたい場合には `:no-frame t` と指定してください。
;;* `rx, ry` ---- 角を丸くしたい場合に、角の x 半径と y 半径を数値で指定します。rx と ry のどちらかだけを指定すると、もう一方も同じであると見なされます。省略した場合のデフォルト値は 0（つまり角を丸くしない）です。
;;* `align` ---- テキストの水平方向のアライメントを `:left :center :right` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
;;* `valign` ---- テキストの垂直方向のアライメントを `:top :center :bottom` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
;;* `margin` ---- テキスト描画における「余白」のサイズです。省略した場合のデフォルト値は 10 です。
;;* `font` ---- フォントを指定します。
;;* `fill` ---- 内部の塗り潰しを指定します。
;;* `stroke` ---- 外枠を描画するストロークを指定します
;;* `link` ---- リンクにする場合、リンク先を指定します。
;;* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
;;* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
;;* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
;;* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
;;* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。
;;
;;${DESCRIPTION}
;;
;;　テキストボックスを描画します。複数のテキストボックスでスタイルを統一したい場合、
;;with-textbox-options マクロを使うことができます。
;;
;;${SEE_ALSO}
;;
;;* テキストボックス
;;* with-textbox-options マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :textbox
 |#
(defmacro textbox (position text &key pivot width height no-frame rx ry
                                    align valign margin font fill stroke
                                    link rotate layer id filter contents)
  (let ((code `(register-entity (make-instance 'textbox
                                               :no-frame ,no-frame
                                               :position ,position :pivot ,pivot
                                               :width ,width :height ,height
                                               :text ,text
                                               :rx     (or ,rx     *default-textbox-rx*)
                                               :ry     (or ,ry     *default-textbox-ry*)
                                               :align  (or ,align  *default-textbox-align*)
                                               :valign (or ,valign *default-textbox-valign*)
                                               :margin (or ,margin *default-textbox-margin*)
                                               :font   (or ,font   *default-textbox-font*)
                                               :fill   (or ,fill   *default-textbox-fill*)
                                               :stroke (or ,stroke *default-textbox-stroke*)
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
;;#### macro with-textbox-options
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-textbox-options}} (${KEY} rx ry align valign margin font fill stroke filter layer) ${BODY} body
;;
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　textbox マクロで描画されるテキストボックスのデフォルトオプションを変更します。キーワード
;;パラメータ群の説明は textbox マクロを参照してください。
;;
;;${SEE_ALSO}
;;
;;* テキストボックス
;;* textbox マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-textbox-options
 |#
(defmacro with-textbox-options ((&key rx ry align valign margin
                                      font fill stroke filter layer) &rest body)
  (labels ((impl (params acc)
             (if (null params)
                 acc
                 (let ((value  (car  params))
                       (symbol (cadr params)))
                   (impl (cddr params)
                         (if (null value)
                             acc
                             (push (list symbol value) acc)))))))
    (let ((lst (impl (list rx     '*default-textbox-rx*
                           ry     '*default-textbox-ry*
                           align  '*default-textbox-align*
                           valign '*default-textbox-valign*
                           margin '*default-textbox-margin*
                           font   '*default-textbox-font*
                           fill   '*default-textbox-fill*
                           stroke '*default-textbox-stroke*
                           filter '*default-textbox-filter*
                           layer  '*default-textbox-layer*) nil)))
      `(let ,lst
         ,@body))))
