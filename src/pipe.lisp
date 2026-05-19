#|
#|ASD|#                (:file "pipe"                      :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "clipping"
#|ASD|#                                                                "path"))
#|EXPORT|#                ;pipe.lisp
 |#

(in-package :kaavio)

#|
#|EXPORT|#                :*default-pipe-depth*
#|EXPORT|#                :*default-pipe-width*
#|EXPORT|#                :*default-pipe-fill*
#|EXPORT|#                :*default-pipe-stroke*
#|EXPORT|#                :*default-pipe-filter*
#|EXPORT|#                :*default-pipe-layer*
 |#
(defparameter *default-pipe-depth*        nil)
(defparameter *default-pipe-width*         20)
(defparameter *default-pipe-fill*         nil)
(defparameter *default-pipe-stroke*       nil)
(defparameter *default-pipe-filter*       nil)
(defparameter *default-pipe-layer*        nil)


(defun pipe-fix-spec (label-spec)
  (when label-spec
    (let ((kaavio::*default-label-offset*   '(0 -5))
          (kaavio::*default-label-position* :above))
      (make-label label-spec))))
  
;;------------------------------------------------------------------------------
;;
;; class pipe
;;
;;------------------------------------------------------------------------------
(defclass pipe (group)
  ((direction :initform nil :initarg :direction)   ; keyword
   (depth     :initform nil :initarg :depth)       ; number
   (label     :initform nil :initarg :label)       ; (or nil label-info)
   (fill      :initform nil :initarg :fill)        ; (or nil fill-info)
   (stroke    :initform nil :initarg :stroke)      ; (or nil stroke-info)
   (clip-path :initform nil :initarg :clip-path)   ; (or nil symbol)
   (filter    :initform nil :initarg :filter)))    ; (or nil keyword)

(defmethod initialize-instance :after ((obj pipe) &rest initargs)
  (declare (ignore initargs))
  (with-slots (depth width label fill stroke filter layer) obj
    (setf depth  (or depth *default-pipe-depth* 10))
    (setf width  (or width *default-pipe-width* 20))
    (setf label  (and label (make-label label)))
    (setf fill   (make-fill   (or fill   *default-pipe-fill*   *default-fill*)))
    (setf stroke (make-stroke (or stroke *default-pipe-stroke* *default-stroke* :none)))
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-pipe-filter* *default-filter*)))
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-pipe-layer* *default-layer*))))
  obj)

(defmethod check ((obj pipe) canvas dict)
  (with-slots (height depth label fill stroke filter clip-path) obj
    (check-member depth     :nullable nil :types number)
    (check-object label     canvas dict :nullable t   :class  label-info)
    (check-object fill      canvas dict :nullable nil :class   fill-info)
    (check-object stroke    canvas dict :nullable nil :class stroke-info)
    (check-member clip-path :nullable   t :types symbol)
    (check-member filter    :nullable   t :types keyword))
  ;; this method must call super class' one.
  (call-next-method))


;; override of group::draw-group
(defmethod draw-group ((obj pipe) writer)
  (let* ((canvas (group-get-canvas obj))
         (w      (canvas-width  canvas))
         (h      (canvas-height canvas)))
    (macrolet ((register-entity (entity)
                 `(check-and-draw-local-entity ,entity canvas writer)))
      (with-slots (direction label depth fill stroke filter clip-path) obj
        (let ((*current-clip-path* clip-path))
          (writer-write writer "<g " (to-property-strings stroke) ">")
          (writer-incr-level writer)
          (let ((*mute-stroke* t))
            ;; draw pipe
            (if (eq direction :v)
                (progn
                  (path `((:move-to (0  0))
                          (:line-to (0 ,h))
                          (:arc-to ,(/ w 2) ,(/ depth 2) 0 0 0 (,w ,h))
                          (:line-to (,w 0))
                          (:arc-to ,(/ w 2) ,(/ depth 2) 0 0 0 (0 0))) :fill fill :filter filter)
                  (path `((:move-to (0  0))
                          (:arc-to ,(/ w 2) ,(/ depth 2) 0 0 0 (,w 0))) :fill :none))
                (progn
                  (path `((:move-to (0  0))
                          (:arc-to ,(/ depth 2) ,(/ h 2) 0 0 0 (0 ,h))
                          (:line-to (,w ,h))
                          (:arc-to ,(/ depth 2) ,(/ h 2) 0 0 0 (,w 0))
                          (:line-to (0 0))) :fill fill :filter filter)
                  (path `((:move-to (,w  0))
                          (:arc-to ,(/ depth 2) ,(/ h 2) 0 0 0 (,w ,h))) :fill :none))))
          (writer-decr-level writer)
          (writer-write writer "</g>")
          ;; draw label
          (when label
            (draw-label label obj clip-path writer))))))
  nil)


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro pipe
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{pipe}} position direction length ${KEY} width pivot depth fill stroke label link rotate layer filter id
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `direction` ---- パイプの向きを `:h` か `:v` で指定します。横方向に伸びるパイプは `:h` 、縦方向なら `:v` です。
;;* `length` ----  パイプの長さを数値で指定します。
;;* `width` ----  パイプの幅を数値で指定します。
;;* `pivot` ----  基準点がパイプのどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `depth` ----  曲線部分のサイズを指定します。
;;* `fill` ----  内部の塗り潰しを指定します。
;;* `stroke` ----  外枠を描画する線を指定します。
;;* `label` ---- ラベルを付ける場合は指定します。
;;* `link` ----  リンクにする場合、リンク先を指定します。
;;* `rotate` ----  回転させたい場合に、その角度を指定します。
;;* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
;;* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
;;* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
;;
;;${DESCRIPTION}
;;
;;　パイプを描画します。複数のパイプでスタイルを統一したい場合、with-pipe-options マクロを
;;使うことができます。
;;
;;${SEE_ALSO}
;;
;;* パイプ
;;* with-pipe-options マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :pipe
 |#
(defmacro pipe (position direction length
                         &key width pivot depth fill stroke label link rotate layer filter id)
  (let ((gw (gensym "WIDTH")))
    `(let ((,gw (or ,width  *default-pipe-width*)))
       (register-entity (make-instance 'kaavio:pipe
                                       :direction ,direction :depth ,depth
                                       :label (pipe-fix-spec ,label)
                                       :fill ,fill :stroke ,stroke :filter ,filter
                                       :clip-path *current-clip-path*
                        #| group  |#   :position ,position :pivot ,pivot
                                       :width  (ecase ,direction ((:h) ,length) ((:v) ,gw))
                                       :height (ecase ,direction ((:h) ,gw) ((:v) ,length))
                        #| shape  |#   :link ,link :rotate ,rotate
                        #| entity |#   :id ,id :layer ,layer)))))



;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-pipe-options
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-pipe-options}} (${KEY} depth width fill stroke filter layer) ${BODY} body
;;
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　pipe マクロで描画されるパイプのデフォルトオプションを変更します。キーワードパラメータ
;;群の説明は pipe マクロを参照してください。
;;
;;${SEE_ALSO}
;;
;;* パイプ
;;* pipe マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-pipe-options
 |#
(defmacro with-pipe-options ((&key (depth  nil depth-p)
                                   (width  nil width-p)
                                   (fill   nil fill-p)
                                   (stroke nil stroke-p)
                                   (filter nil filter-p)
                                   (layer  nil layer-p)) &rest body)
  (let ((bindings nil))
    (labels ((impl (arg-p binding)
               (when arg-p (push binding bindings))))
      (impl depth-p  `(*default-pipe-depth*  ,depth))
      (impl width-p  `(*default-pipe-width*  ,width))
      (impl fill-p   `(*default-pipe-fill*   (make-fill2   *default-pipe-fill*   ,fill)))
      (impl stroke-p `(*default-pipe-stroke* (make-stroke2 *default-pipe-stroke* ,stroke)))
      (impl filter-p `(*default-pipe-filter* ,filter))
      (impl layer-p  `(*default-pipe-layer*  ,layer)))
    `(let ,(nreverse bindings)
       ,@body)))
