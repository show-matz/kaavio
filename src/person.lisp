#|
#|ASD|#                (:file "person"                    :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "clipping"
#|ASD|#                                                                "path"))
#|EXPORT|#                ;person.lisp
 |#

(in-package :kaavio)

#|
#|EXPORT|#                :*default-person-fill*
#|EXPORT|#                :*default-person-stroke*
#|EXPORT|#                :*default-person-filter*
#|EXPORT|#                :*default-person-layer*
 |#
(defparameter *default-person-fill*         nil)
(defparameter *default-person-stroke*       nil)
(defparameter *default-person-filter*       nil)
(defparameter *default-person-layer*        nil)


;;------------------------------------------------------------------------------
;;
;; class person
;;
;;------------------------------------------------------------------------------
(defclass person (group)
  ((label   :initform nil :initarg :label)       ; (or nil label-info)
   (fill    :initform nil :initarg :fill)        ; (or nil fill-info)
   (stroke  :initform nil :initarg :stroke)      ; (or nil stroke-info)
   (clip-path :initform nil :initarg :clip-path) ; (or nil symbol)
   (filter  :initform nil :initarg :filter)))    ; (or nil keyword)
  
(defmethod initialize-instance :after ((prsn person) &rest initargs)
  (declare (ignore initargs))
  (with-slots (label fill stroke filter layer) prsn
    (setf label  (and label (make-label label)))
    (setf fill   (make-fill   (or fill   *default-person-fill*   *default-fill*)))
    (setf stroke (make-stroke (or stroke *default-person-stroke* *default-stroke* :none)))
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-person-filter* *default-filter*)))
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-person-layer* *default-layer*))))
  prsn)

(defmethod check ((prsn person) canvas dict)
  (with-slots (label fill stroke filter clip-path) prsn
    (check-object   label   canvas dict :nullable t   :class  label-info)
    (check-object   fill    canvas dict :nullable nil :class   fill-info)
    (check-object   stroke  canvas dict :nullable nil :class stroke-info)
    (check-member clip-path :nullable   t :types symbol)
    (check-member   filter  :nullable   t :types keyword))
  ;; this method must call super class' one.
  (call-next-method))


;; override of group::draw-group
(defmethod draw-group ((prsn person) writer)
  (let ((canvas (group-get-canvas prsn)))
    (with-canvas (cc w h) canvas
      (macrolet ((register-entity (entity)
                   `(check-and-draw-local-entity ,entity canvas writer)))
        (with-slots (label fill stroke filter clip-path) prsn
          ;; draw person
          (let ((w/2 (/ w 2))
                (h/2 (/ h 2))
                (*current-clip-path* clip-path))
            (path `((:move-to ,(xy+ cc (- w/2) h/2))
                    (:arc-to ,w/2 ,h/2 0 1 1 ,(xy+ cc w/2 h/2))
                    (:line-to ,(xy+ cc (- w/2) h/2))
                    (:move-to ,cc)
                    (:arc-to ,w/2 ,w/2 0 0 1 ,(y+ cc (- h/2)))
                    (:arc-to ,w/2 ,w/2 0 0 1 ,cc))
                  :fill fill :stroke stroke :filter filter))
          ;; draw label
          (when label
            (draw-label label prsn clip-path writer))))))
  nil)


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro person
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{person}} position size ${KEY} pivot fill stroke label link rotate layer filter id
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `size` ---- 幅を数値で指定します。高さは自動的にこの 2 倍になります。
;;* `pivot` ---- 基準点が人物のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `fill` ---- 内部の塗り潰しを指定します。
;;* `stroke` ---- 外枠を描画するストロークを指定します。
;;* `label` ---- ラベルを付ける場合は指定します。
;;* `link` ---- リンクにする場合、リンク先を指定します。
;;* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
;;* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
;;* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
;;* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。
;;
;;${DESCRIPTION}
;;
;;　人物を描画します。複数の人物でスタイルを統一したい場合、with-person-options マクロを
;;使うことができます。
;;
;;${SEE_ALSO}
;;
;;* 人物
;;* with-person-options マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :person
 |#
(defmacro person (position size
                        &key pivot fill stroke label link rotate layer filter id)
  `(register-entity (make-instance 'kaavio:person
                                   :label ,label :fill ,fill
                                   :stroke ,stroke :filter ,filter
                                   :clip-path *current-clip-path*
                    #| group  |#   :position ,position :pivot ,pivot :width ,size :height ,(* 2 size)
                    #| shape  |#   :link ,link :rotate ,rotate
                    #| entity |#   :id ,id :layer ,layer)))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-person-options
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-person-options}} (${KEY} fill stroke layer filter) ${BODY} body
;;
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　person マクロで描画される人物のデフォルトオプションを変更します。キーワードパラメータ
;;群の説明は person マクロを参照してください。
;;
;;${SEE_ALSO}
;;
;;* 人物
;;* person マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-person-options
 |#
(defmacro with-person-options ((&key (fill   nil fill-p)
                                     (stroke nil stroke-p)
                                     (filter nil filter-p)
                                     (layer  nil layer-p)) &rest body)
  (let ((bindings nil))
    (labels ((impl (arg-p binding)
               (when arg-p (push binding bindings))))
      (impl fill-p   `(*default-person-fill*   (make-fill2   *default-person-fill*   ,fill)))
      (impl stroke-p `(*default-person-stroke* (make-stroke2 *default-person-stroke* ,stroke)))
      (impl filter-p `(*default-person-filter* ,filter))
      (impl layer-p  `(*default-person-layer*  ,layer)))
    `(let ,(nreverse bindings)
       ,@body)))
