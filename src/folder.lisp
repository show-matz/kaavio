#|
#|ASD|#                (:file "folder"                    :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "polygon"
#|ASD|#                                                                "filter"
#|ASD|#                                                                "text-shape"))
#|EXPORT|#                ;folder.lisp
 |#

(in-package :kaavio)

#|
#|EXPORT|#                :*default-folder-tabwidth*
#|EXPORT|#                :*default-folder-tabheight*
#|EXPORT|#                :*default-folder-align*
#|EXPORT|#                :*default-folder-valign*
#|EXPORT|#                :*default-folder-margin*
#|EXPORT|#                :*default-folder-font*
#|EXPORT|#                :*default-folder-fill*
#|EXPORT|#                :*default-folder-stroke*
#|EXPORT|#                :*default-folder-filter*
#|EXPORT|#                :*default-folder-layer*
 |#
(defparameter *default-folder-tabwidth*     50)
(defparameter *default-folder-tabheight*    20)
(defparameter *default-folder-align*        :center)
(defparameter *default-folder-valign*       :center)
(defparameter *default-folder-margin*       10)
(defparameter *default-folder-font*         nil)
(defparameter *default-folder-fill*         nil)
(defparameter *default-folder-stroke*       nil)
(defparameter *default-folder-filter*       nil)
(defparameter *default-folder-layer*        nil)

;;------------------------------------------------------------------------------
;;
;; class folder
;;
;;------------------------------------------------------------------------------
(defclass folder (text-shape)
  ((tab-width   :initform nil :initarg :tab-width)    ; number
   (tab-height  :initform nil :initarg :tab-height)   ; number
   (filter      :initform nil :initarg :filter)))     ; (or nil keyword)

(defmethod initialize-instance :after ((fldr folder) &rest initargs)
  (declare (ignore initargs))
  (with-slots (filter layer) fldr
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-folder-filter* *default-filter*)))
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-folder-layer* *default-layer*))))
  fldr)
  
(defmethod check ((fldr folder) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (filter) fldr
    (check-member filter    :nullable   t :types keyword))
  nil)

;; override of group::draw-group
(defmethod draw-group ((box folder) writer)
  (let* ((canvas (group-get-canvas box))
         (width  (canvas-width  canvas))
         (height (canvas-height canvas)))
    (macrolet ((register-entity (entity)
                 `(check-and-draw-local-entity ,entity canvas writer)))
      (with-slots (tab-width tab-height fill stroke filter) box
        (let ((tab-w tab-width)
              (tab-h tab-height))
          (writer-write writer "<g " (to-property-strings fill)
                                     (to-property-strings stroke) ">")
          (writer-incr-level writer)
          (let ((*mute-fill* t)
                (*mute-stroke* t))
            ;; draw box
            (polygon `((0 0) (0 ,height)
                       (,width ,height)
                       (,width ,(/ tab-h 2))
                       (,(- width (/ tab-h 2)) 0)
                       (,tab-w 0)
                       (,(- tab-w (/ tab-h 2)) ,(- (/ tab-h 2)))
                       (,(/ tab-h 2) ,(- (/ tab-h 2)))
                       (0 0)) :fill fill :stroke stroke :filter filter)
            (polygon `((0 0)
                       (,(/ tab-h 2) ,(/ tab-h 2))
                       (,(- tab-w (/ tab-h 2)) ,(/ tab-h 2))
                       (,tab-w 0)
                       (,(- tab-w (/ tab-h 2)) ,(- (/ tab-h 2)))
                       (,(/ tab-h 2) ,(- (/ tab-h 2)))
                       (0 0)) :fill fill :stroke stroke :filter :none))
          (writer-decr-level writer)
          (writer-write writer "</g>")))))
  ;; draw text
  (call-next-method))

;; no override.
;(defmethod text-shape-calc-size ((box folder))
;  (call-next-method))

;; no override.
;(defmethod text-shape-paragraph-area ((box folder))
;  (call-next-method))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro folder
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{folder}} position text ${KEY} pivot width height tab-width tab-height align valign font fill stroke margin link rotate layer filter id contents
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `text` ---- 内部に描画するテキストを文字列で指定します。改行は "~%" で表現します。
;;* `pivot` ---- 基準点がフォルダのどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `width` ---- 幅を自動決定せず、明示的に指定したい場合に数値で指定します。
;;* `height` ---- 高さを自動決定せず、明示的に指定したい場合に数値で指定します。
;;* `tab-width` ---- 左上に描画されるタブ部分の幅を指定します。省略した場合のデフォルト値は 50 です。
;;* `tab-height` ---- 左上に描画されるタブ部分の高さを指定します。省略した場合のデフォルト値は 20 です。
;;* `align` ---- テキストの水平方向のアライメントを `:left :center :right` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
;;* `valign` ---- テキストの垂直方向のアライメントを `:top :center :bottom` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
;;* `font` ---- フォントを指定します。
;;* `fill` ---- 内部の塗り潰しを指定します。
;;* `stroke` ---- 外枠を描画するストロークを指定します。
;;* `margin` ---- テキスト描画における「余白」のサイズです。省略した場合のデフォルト値は 10 です。
;;* `link` ---- リンクにする場合、リンク先を指定します。
;;* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
;;* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
;;* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
;;* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。
;;* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。
;;
;;${DESCRIPTION}
;;
;;　フォルダを描画します。複数のフォルダでスタイルを統一したい場合、
;;with-folder-options マクロを使うことができます。
;;
;;${SEE_ALSO}
;;
;;* フォルダ
;;* with-folder-options マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :folder
 |#
(defmacro folder (position text &key pivot width height tab-width tab-height align
                                   valign font fill stroke margin link rotate layer filter id contents)
  (let ((code `(register-entity (make-instance 'folder
                                               :position ,position :pivot ,pivot
                                               :width ,width :height ,height
                                               :text ,text
                                               :tab-width  (or ,tab-width   *default-folder-tabwidth*)
                                               :tab-height (or ,tab-height  *default-folder-tabheight*)
                                               :align  (or ,align  *default-folder-align*)
                                               :valign (or ,valign *default-folder-valign*)
                                               :margin (or ,margin *default-folder-margin*)
                                               :font   (or ,font   *default-folder-font*)
                                               :fill   (or ,fill   *default-folder-fill*)
                                               :stroke (or ,stroke *default-folder-stroke*)
                                               :link ,link :rotate ,rotate
                                               :filter ,filter :layer ,layer :id ,id))))
    (if (null contents)
        code
        (let ((g-obj (gensym "OBJ")))
          `(let* ((,g-obj ,code)
                  (canvas (kaavio:shape-get-subcanvas ,g-obj)))
             (declare (special canvas))
             ,@contents)))))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-folder-options
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-folder-options}} (${KEY} tab-width tab-height align valign margin font fill stroke filter layer) ${BODY} body
;;
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　folder マクロで描画されるフォルダのデフォルトオプションを変更します。キーワードパラメータ
;;群の説明は folder マクロを参照してください。
;;
;;${SEE_ALSO}
;;
;;* フォルダ
;;* folder マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-folder-options
 |#
(defmacro with-folder-options ((&key tab-width tab-height align valign
                                     margin font fill stroke filter layer) &rest body)
  (labels ((impl (params acc)
             (if (null params)
                 acc
                 (let ((value  (car  params))
                       (symbol (cadr params)))
                   (impl (cddr params)
                         (if (null value)
                             acc
                             (push (list symbol value) acc)))))))
    (let ((lst (impl (list tab-width  '*default-folder-tabwidth*
                           tab-height '*default-folder-tabheight*
                           align      '*default-folder-align*
                           valign     '*default-folder-valign*
                           margin     '*default-folder-margin*
                           font       '*default-folder-font*
                           fill       '*default-folder-fill*
                           stroke     '*default-folder-stroke*
                           filter     '*default-folder-filter*
                           layer      '*default-folder-layer*) nil)))
      `(let ,lst
         ,@body))))
