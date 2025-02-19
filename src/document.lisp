#|
#|ASD|#                (:file "document"                  :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "path"
#|ASD|#                                                                "filter"
#|ASD|#                                                                "text-shape"))
#|EXPORT|#                ;document.lisp
 |#

(in-package :kaavio)

#|
#|EXPORT|#                :*default-document-align*
#|EXPORT|#                :*default-document-valign*
#|EXPORT|#                :*default-document-margin*
#|EXPORT|#                :*default-document-font*
#|EXPORT|#                :*default-document-fill*
#|EXPORT|#                :*default-document-stroke*
#|EXPORT|#                :*default-document-filter*
#|EXPORT|#                :*default-document-layer*
 |#
(defparameter *default-document-align*  :center)
(defparameter *default-document-valign* :center)
(defparameter *default-document-margin* 10)
(defparameter *default-document-font*   nil)
(defparameter *default-document-fill*   nil)
(defparameter *default-document-stroke* nil)
(defparameter *default-document-filter* nil)
(defparameter *default-document-layer*  nil)


;;------------------------------------------------------------------------------
;;
;; class document
;;
;;------------------------------------------------------------------------------
(defclass document (text-shape)
  ((depth  :initform nil :initarg :depth)    ; number
   (filter :initform nil :initarg :filter))) ; (or nil keyword)
  
(defmethod initialize-instance :after ((doc document) &rest initargs)
  (declare (ignore initargs))
  (with-slots (filter layer) doc
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-document-layer* *default-layer*)))
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-document-filter* *default-filter*))))
  doc)
   
(defmethod check ((doc document) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (depth filter height) doc
    (setf depth (or depth (/ height 3)))
    (check-member depth  :nullable nil :types number)
    (check-member filter :nullable   t :types keyword))
  nil)

;; override of group::draw-group
(defmethod draw-group ((doc document) writer)
  (let* ((canvas (group-get-canvas doc))
         (w      (canvas-width  canvas))
         (h      (canvas-height canvas)))
    (macrolet ((register-entity (entity)
                 `(check-and-draw-local-entity ,entity canvas writer)))
      (with-slots (depth fill stroke filter) doc
        ;; draw 
        (path `((:move-to ( 0  0))
                (:line-to ( 0 ,h))
                (:arc-to  ,(/ w 3) ,depth 0 0 0 (,(/ w 2) ,h))
                (:arc-to  ,(/ w 3) ,depth 1 0 1 (,w ,h))
                (:line-to (,w 0)) :close-path) :fill fill :stroke stroke :filter filter))))
  ;; draw text
  (call-next-method))

;; no override.
;(defmethod text-shape-calc-size ((doc document))
;  (call-next-method))

;; no override.
;(defmethod text-shape-paragraph-area ((doc document))
;  (call-next-method))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro document
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{document}} position width height text ${KEY} pivot depth align valign margin font fill stroke link rotate layer id filter contents
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `width` ---- 幅を数値で指定します。
;;* `height` ---- 高さを数値で指定します。
;;* `text` ---- 内部に描画するテキストを文字列で指定します。改行は "~%" で表現します。
;;* `pivot` ---- 基準点がドキュメントのどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `depth` ---- 下部の曲線の深さを指定します。省略した場合のデフォルト値は height の 1/3 です。
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
;;　ドキュメントを描画します。複数のドキュメントでスタイルを統一したい場合、
;;with-document-options マクロを使うことができます。
;;
;;${SEE_ALSO}
;;
;;* ドキュメント
;;* with-document-options マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :document
 |#
(defmacro document (position width height text
                         &key pivot depth align valign margin
                              font fill stroke link rotate layer id filter contents)
  (let ((code `(register-entity (make-instance 'document
                                               :position ,position :pivot ,pivot :depth ,depth
                                               :width ,width :height ,height
                                               :text ,text
                                               :align  (or ,align  *default-document-align*)
                                               :valign (or ,valign *default-document-valign*)
                                               :margin (or ,margin *default-document-margin*)
                                               :font   (or ,font   *default-document-font*)
                                               :fill   (or ,fill   *default-document-fill*)
                                               :stroke (or ,stroke *default-document-stroke*)
                                               :link ,link  :rotate ,rotate
                                               :filter ,filter :layer ,layer :id ,id))))
    (if (null contents)
        code
        (let ((g-obj (gensym "OBJ")))
          `(let* ((,g-obj ,code)
                  (canvas (kaavio:shape-get-subcanvas ,g-obj)))
             (declare (special canvas))
             ,@contents)))))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-document-options
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-document-options}} (${KEY} align valign margin font fill stroke filter layer) ${BODY} body
;;
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　document マクロで描画されるドキュメントのデフォルトオプションを変更します。キーワード
;;パラメータ群の説明は document マクロを参照してください。
;;
;;${SEE_ALSO}
;;
;;* ドキュメント
;;* document マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-document-options
 |#
(defmacro with-document-options ((&key align valign margin
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
    (let ((lst (impl (list align  '*default-document-align*
                           valign '*default-document-valign*
                           margin '*default-document-margin*
                           font   '*default-document-font*
                           fill   '*default-document-fill*
                           stroke '*default-document-stroke*
                           filter '*default-document-filter*
                           layer  '*default-document-layer*) nil)))
      `(let ,lst
         ,@body))))

