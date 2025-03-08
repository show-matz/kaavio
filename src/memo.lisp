#|
#|ASD|#                (:file "memo"                      :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "arc"
#|ASD|#                                                                "polygon"
#|ASD|#                                                                "filter"
#|ASD|#                                                                "text-shape"))
#|EXPORT|#                ;memo.lisp
 |#

(in-package :kaavio)

#|
#|EXPORT|#                :*default-memo-crease*
#|EXPORT|#                :*default-memo-align*
#|EXPORT|#                :*default-memo-valign*
#|EXPORT|#                :*default-memo-margin*
#|EXPORT|#                :*default-memo-font*
#|EXPORT|#                :*default-memo-fill*
#|EXPORT|#                :*default-memo-fill2*
#|EXPORT|#                :*default-memo-stroke*
#|EXPORT|#                :*default-memo-filter*
#|EXPORT|#                :*default-memo-layer*
 |#
(defparameter *default-memo-crease*   20)
(defparameter *default-memo-align*    :center)
(defparameter *default-memo-valign*   :center)
(defparameter *default-memo-margin*   10)
(defparameter *default-memo-font*     nil)
(defparameter *default-memo-fill*     nil)
(defparameter *default-memo-fill2*    nil)
(defparameter *default-memo-stroke*   nil)
(defparameter *default-memo-filter*   nil)
(defparameter *default-memo-layer*    nil)


(defun memo-get-points1 (w h c)
  `((0         0)
    (0        ,h)
    (,(- w c) ,h)
    (,(- w (/ c 1.5)) ,(- h (/ c 1.5)))
    (,w       ,(- h (/ c 2)))
    (,w        0)))

(defun memo-get-points2 (w h c)
  `((,(- w c)       ,h)
    (,w             ,(- h (/ c 2)))
    (,(- w (/ c 1.5)) ,(- h (/ c 1.5)))))

(defun memo-get-stroke-points1 (w h c)
  `((0        ,(/ h 2))
    (0         0)
    (,w        0)
    (,w       ,(- h (/ c 2)))
    (,(- w c) ,h)
    (0        ,h)
    (0        ,(/ h 2))))

(defun memo-get-stroke-points2 (w h c)
  `((,w       ,(- h (/ c 2)))
    (,(- w (/ c 1.5)) ,(- h (/ c 1.5)))
    (,(- w c) ,h)))

;;------------------------------------------------------------------------------
;;
;; class memo
;;
;;------------------------------------------------------------------------------
(defclass memo (text-shape)
  ((crease  :initform nil :initarg :crease)   ; number
   (fill2   :initform nil :initarg :fill2)    ; (or nil fill-info)
   (filter  :initform nil :initarg :filter))) ; (or nil keyword)
  
(defmethod initialize-instance :after ((obj memo) &rest initargs)
  (declare (ignore initargs))
  (with-slots (crease fill2 filter layer) obj
    (setf crease (or crease *default-memo-crease*))
    (setf fill2  (make-fill (or fill2 *default-fill* :none)))
    (setf filter  (if (eq filter :none)
                      nil
                      (or filter *default-memo-filter* *default-filter*)))
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-memo-layer* *default-layer*))))
  obj)
   
(defmethod check ((obj memo) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (crease fill fill2 filter) obj
    (check-member crease    :nullable nil :types number)
    (setf fill2 (or fill2 fill))
    (check-object fill2  canvas dict :nullable t :class fill-info)
    (check-member filter    :nullable   t :types keyword))
  nil)

;; override of group::draw-group
(defmethod draw-group ((obj memo) writer)
  (let* ((canvas (group-get-canvas obj))
         (width  (canvas-width  canvas))
         (height (canvas-height canvas)))
    (macrolet ((register-entity (entity)
                 `(check-and-draw-local-entity ,entity canvas writer)))
      (with-slots (crease fill fill2 stroke filter clip-path) obj
        ;; draw
        (let ((*current-clip-path* clip-path))
          (writer-write writer "<g stroke='none' "
                        (write-when filter "filter='url(#" it ")' ") ">")
          (writer-incr-level writer)
          (let ((*mute-stroke* t))
            (polygon (memo-get-points1 width height crease) :fill fill)
            (polygon (memo-get-points2 width height crease) :fill fill2))
          (writer-write writer "<g fill='none' "
                        (to-property-strings stroke) ">")
          (writer-incr-level writer)
          (let ((*mute-stroke* t))
            (line (memo-get-stroke-points1 width height crease))
            (line (memo-get-stroke-points2 width height crease)))
          (writer-decr-level writer)
          (writer-write writer "</g>")
          (writer-decr-level writer)
          (writer-write writer "</g>")))))
  ;; draw text
  (call-next-method))

;; no override.
;(defmethod text-shape-calc-size ((obj memo))
;  (call-next-method))

;; no override.
;(defmethod text-shape-paragraph-area ((obj memo))
;  (call-next-method))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro memo
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{memo}} position text ${KEY} pivot width height crease align valign margin font fill fill2 stroke link rotate layer id filter contents
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `text` ---- 内部に描画するテキストを文字列で指定します。改行は "~%" で表現します。
;;* `pivot` ---- 基準点がメモのどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `width` ---- 幅を自動決定せず、明示的に指定したい場合に数値で指定します。
;;* `height` ---- 高さを自動決定せず、明示的に指定したい場合に数値で指定します。
;;* `crease` ---- 右下の折り目のサイズを数値で指定します。省略した場合のデフォルト値は 20 です。
;;* `align` ---- テキストの水平方向のアライメントを `:left :center :right` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
;;* `valign` ---- テキストの垂直方向のアライメントを `:top :center :bottom` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
;;* `margin` ---- テキスト描画における「余白」のサイズです。省略した場合のデフォルト値は 10 です。
;;* `font` ---- フォントを指定します。
;;* `fill` ---- 内部の塗り潰しを指定します。
;;* `fill2` ---- 折り目部分の塗り潰しを指定します。
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
;;　メモを描画します。複数のメモでスタイルを統一したい場合、with-memo-options マクロを
;;使うことができます。
;;
;;${SEE_ALSO}
;;
;;* メモ
;;* with-memo-options マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :memo
 |#
(defmacro memo (position text &key pivot width height crease align valign margin
                                 font fill fill2 stroke link rotate layer id filter contents)
  (let* ((g-fill (gensym "FILL"))
         (code `(let ((,g-fill ,fill))
                  (register-entity (make-instance 'memo
                                                   :crease ,crease
                                                   :position ,position :pivot ,pivot
                                                   :width ,width :height ,height
                                                   :text ,text
                                                   :align  (or ,align  *default-memo-align*)
                                                   :valign (or ,valign *default-memo-valign*)
                                                   :margin (or ,margin *default-memo-margin*)
                                                   :font   (or ,font   *default-memo-font*)
                                                   :fill   (or ,g-fill *default-memo-fill*)
                                                   :fill2  (or ,fill2
                                                               ,g-fill *default-memo-fill2*
                                                                       *default-memo-fill*)
                                                   :stroke (or ,stroke *default-memo-stroke*)
                                                   :link ,link :rotate ,rotate
                                                   :clip-path *current-clip-path*
                                                   :filter ,filter :layer ,layer :id ,id)))))
    (if (null contents)
        code
        (let ((g-obj (gensym "OBJ")))
          `(let* ((,g-obj ,code)
                  (canvas (kaavio:shape-get-subcanvas ,g-obj)))
             (declare (special canvas))
             ,@contents)))))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-memo-options
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-memo-options}} (${KEY} crease align valign margin font fill fill2 stroke filter layer) ${BODY} body
;;
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　memo マクロで描画されるメモのデフォルトオプションを変更します。キーワードパラメータ
;;群の説明は memo マクロを参照してください。
;;
;;${SEE_ALSO}
;;
;;* メモ
;;* memo マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-memo-options
 |#
(defmacro with-memo-options ((&key crease align valign margin
                                   font fill fill2 stroke filter layer) &rest body)
  (labels ((impl (params acc)
             (if (null params)
                 acc
                 (let ((value  (car  params))
                       (symbol (cadr params)))
                   (impl (cddr params)
                         (if (null value)
                             acc
                             (push (list symbol value) acc)))))))
    (let ((lst (impl (list crease '*default-memo-crease*
                           align  '*default-memo-align*
                           valign '*default-memo-valign*
                           margin '*default-memo-margin*
                           font   '*default-memo-font*
                           fill   '*default-memo-fill*
                           fill2  '*default-memo-fill2*
                           stroke '*default-memo-stroke*
                           filter '*default-memo-filter*
                           layer  '*default-memo-layer*) nil)))
      `(let ,lst
         ,@body))))
