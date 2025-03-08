#|
#|ASD|#                (:file "cube"                      :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "path"
#|ASD|#                                                                "filter"
#|ASD|#                                                                "text-shape"))
#|EXPORT|#                ;cube.lisp
 |#

(in-package :kaavio)

#|
#|EXPORT|#                :*default-cube-depth*
#|EXPORT|#                :*default-cube-align*
#|EXPORT|#                :*default-cube-valign*
#|EXPORT|#                :*default-cube-margin*
#|EXPORT|#                :*default-cube-font*
#|EXPORT|#                :*default-cube-fill*
#|EXPORT|#                :*default-cube-fill2*
#|EXPORT|#                :*default-cube-stroke*
#|EXPORT|#                :*default-cube-filter*
#|EXPORT|#                :*default-cube-layer*
 |#
(defparameter *default-cube-depth*         nil)
(defparameter *default-cube-align*     :center)
(defparameter *default-cube-valign*    :center)
(defparameter *default-cube-margin*         10)
(defparameter *default-cube-font*          nil)
(defparameter *default-cube-fill*          nil)
(defparameter *default-cube-fill2*         nil)
(defparameter *default-cube-stroke*        nil)
(defparameter *default-cube-filter*        nil)
(defparameter *default-cube-layer*         nil)

;;------------------------------------------------------------------------------
;;
;; class cube
;;
;;------------------------------------------------------------------------------
(defclass cube (text-shape)
  ((depth   :initform nil :initarg :depth)    ; number
   (fill2   :initform nil :initarg :fill2)    ; (or nil fill-info)
   (filter  :initform nil :initarg :filter))) ; (or nil keyword)

(defmethod initialize-instance :after ((cb cube) &rest initargs)
  (declare (ignore initargs))
  (with-slots (fill2 filter layer) cb
    (setf fill2  (make-fill (or fill2 *default-fill* :none)))
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-cube-filter* *default-filter*)))
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-cube-layer* *default-layer*))))
  cb)

(defmethod check ((cb cube) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (depth fill fill2 filter height) cb
    (setf depth (or depth (/ height 5)))
    (check-member depth  :nullable nil :types number)
    (setf fill2 (or fill2 fill))
    (check-object fill2  canvas dict :nullable t :class fill-info)
    (check-member filter :nullable   t :types keyword))
  nil)

(defmethod shape-get-subcanvas ((cb cube))
  (with-slots (depth) cb
    (let ((half (/ depth 2)))
      (make-canvas (point/y+ (attribute-topleft cb) half)
                   (- (attribute-width  cb) half)
                   (- (attribute-height cb) half)))))

;; override of group::draw-group
(defmethod draw-group ((cb cube) writer)
  (let ((canvas (group-get-canvas cb)))
    (with-slots (depth contents-p fill fill2 stroke filter clip-path) cb
      (let* ((width     (canvas-width  canvas))
             (height    (canvas-height canvas))
             (x         (/ width  2))
             (y         (/ height 2))
             (half      (/ depth  2))
             (*current-clip-path* clip-path))
        (macrolet ((register-entity (entity)
                     `(check-and-draw-local-entity ,entity canvas writer)))
          (writer-write writer "<g stroke='none' "
                               (write-when filter "filter='url(#" it ")' ") ">")
          (writer-incr-level writer)
          (let ((*mute-stroke* t))
            (polygon `((0 ,half)
                       (,depth ,(- half))
                       (,(+ width half) ,(- half))
                       (,(+ width half) ,(- height depth))
                       (,(- width half) ,height)
                       (,(- width half) ,half)) :fill fill2)
            (rectangle (make-point (- x (/ half 2))
                                   (+ y (/ half 2)))
                       (- width half) (- height half) :fill fill))
          (path `((:move-to (0 ,half))
                  (:line-to (,(- width half) ,half)
                            (,(- width half) ,height)
                            (0               ,height)
                            (0               ,half)
                            (,depth          ,(- half))
                            (,(+ width half) ,(- half))
                            (,(+ width half) ,(- height depth))
                            (,(- width half) ,height))
                  (:move-to (,(- width half) ,half))
                  (:line-to (,(+ width half) ,(- half))))
                :stroke stroke :fill :none)
          (writer-decr-level writer)
          (writer-write writer "</g>")))))
  ;; draw text
  (call-next-method))

;; no override.
;(defmethod text-shape-calc-size ((cb cube))
;  (call-next-method))

(defmethod text-shape-paragraph-area ((cb cube))
  (shape-get-subcanvas cb))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro cube
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{cube}} position width height text ${KEY} pivot depth align valign margin font fill fill2 stroke link rotate layer id filter contents
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `width` ----  幅を数値で指定します。
;;* `height` ----  高さを数値で指定します。
;;* `text` ----  内部に描画するテキストを文字列で指定します。改行は "~%" で表現します。
;;* `pivot` ----  基準点がキューブのどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `depth` ----  上面および側面の大きさを数値で指定します。省略した場合のデフォルト値は height の 1/5 です。
;;* `align` ----  テキストの水平方向のアライメントを `:left :center :right` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
;;* `valign` ----  テキストの垂直方向のアライメントを `:top :center :bottom` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
;;* `margin` ----  テキスト描画における「余白」のサイズです。省略した場合のデフォルト値は 10 です。
;;* `font` ---- フォントを指定します。
;;* `fill` ---- 内部の塗り潰しを指定します。
;;* `fill2` ----  上面および側面の塗り潰しを指定します。
;;* `stroke` ----  外枠を描画する線を指定します。
;;* `link` ---- リンクにする場合、リンク先を指定します。
;;* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
;;* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
;;* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
;;* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
;;* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。
;;
;;${DESCRIPTION}
;;
;;　キューブを描画します。複数のキューブでスタイルを統一したい場合、with-cube-options マクロを
;;使うことができます。
;;
;;${SEE_ALSO}
;;
;;* キューブ
;;* with-cube-options マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :cube
 |#
(defmacro cube (position width height text
                    &key pivot depth align valign margin
                         font fill fill2 stroke link rotate layer id filter contents)
  (let* ((g-fill (gensym "FILL"))
         (code `(let ((,g-fill ,fill))
                  (register-entity (make-instance 'cube
                                                   :position ,position :pivot ,pivot :text ,text
                                                   :width ,width :height ,height
                                                   :depth  (or ,depth  *default-cube-depth*)
                                                   :align  (or ,align  *default-cube-align*)
                                                   :valign (or ,valign *default-cube-valign*)
                                                   :margin (or ,margin *default-cube-margin*)
                                                   :font   (or ,font   *default-cube-font*)
                                                   :fill   (or ,g-fill *default-cube-fill*)
                                                   :fill2  (or ,fill2
                                                               ,g-fill *default-cube-fill2*
                                                                       *default-cube-fill*)
                                                   :stroke (or ,stroke *default-cube-stroke*)
                                                   :link ,link  :rotate ,rotate
                                                   :clip-path *current-clip-path*
                                                   :filter ,filter :layer ,layer :id ,id)))))
    (if (null contents)
        code
        (let ((g-obj (gensym "OBJ")))
          `(let* ((,g-obj ,code)
                  (canvas (shape-get-subcanvas ,g-obj)))
             (declare (special canvas))
             ,@contents)))))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-cube-options
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-cube-options}} (${KEY} depth align valign margin font fill fill2 stroke filter layer) ${BODY} body
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　cube マクロで描画されるキューブのデフォルトオプションを変更します。キーワードパラメータ
;;群の説明は cube マクロを参照してください。
;;
;;${SEE_ALSO}
;;
;;* キューブ
;;* cube マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-cube-options
 |#
(defmacro with-cube-options ((&key depth align valign margin
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
    (let ((lst (impl (list depth  '*default-cube-depth*
                           align  '*default-cube-align*
                           valign '*default-cube-valign*
                           margin '*default-cube-margin*
                           font   '*default-cube-font*
                           fill   '*default-cube-fill*
                           fill2  '*default-cube-fill2*
                           stroke '*default-cube-stroke*
                           filter '*default-cube-filter*
                           layer  '*default-cube-layer*) nil)))
      `(let ,lst
         ,@body))))
