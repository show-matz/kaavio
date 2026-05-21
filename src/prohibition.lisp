#|
#|ASD|#                (:file "prohibition"               :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "clipping"
#|ASD|#                                                                "path"))
#|EXPORT|#                ;prohibition.lisp
 |#

(in-package :kaavio)

#|
#|EXPORT|#                :*default-prohibition-fill*
#|EXPORT|#                :*default-prohibition-stroke*
#|EXPORT|#                :*default-prohibition-filter*
#|EXPORT|#                :*default-prohibition-layer*
 |#
(defparameter *default-prohibition-fill*         nil)
(defparameter *default-prohibition-stroke*       nil)
(defparameter *default-prohibition-filter*       nil)
(defparameter *default-prohibition-layer*        nil)


(defun prohibition-fix-spec (label-spec)
  (when label-spec
    (let ((kaavio::*default-label-offset*   '(0 15))
          (kaavio::*default-label-position* :below))
      (make-label label-spec))))

(defun prohibition-get-points (position size bar-width)
  (let* ((pts   nil)
         (r1    (/ size 2))
         (r2    (- r1 bar-width))
         (theta (/ (cl:asin (/ bar-width (* 2 r2))) (/ pi 180))))
    (labels ((add-pt (degree r)
               (push (xy+ position (* (math/cos1 degree) r)
                                   (* (math/sin1 degree) r)) pts)))
      ;; outer circle
      (add-pt    270        r1)  ;; move-to
      (add-pt     90        r1)  ;; arc-to
      (add-pt    270        r1)  ;; arc-to
      ;; inner circle & bar
      (add-pt (- 225 theta) r2)  ;; move-to
      (add-pt (+  45 theta) r2)  ;; arc-to
      (add-pt (- 225 theta) r2)  ;; line-to
      (add-pt (+ 225 theta) r2)  ;; move-to
      (add-pt (-  45 theta) r2)  ;; arc-to
      (add-pt (+ 225 theta) r2)) ;; line-to
    (values r1 r2 (nreverse pts))))

;;------------------------------------------------------------------------------
;;
;; class prohibition
;;
;;------------------------------------------------------------------------------
(defclass prohibition (group)
  ((bar-width :initform nil :initarg :bar-width)   ; number
   (label     :initform nil :initarg :label)       ; (or nil label-info)
   (fill      :initform nil :initarg :fill)        ; (or nil fill-info)
   (stroke    :initform nil :initarg :stroke)      ; (or nil stroke-info)
   (clip-path :initform nil :initarg :clip-path)   ; (or nil symbol)
   (filter    :initform nil :initarg :filter)))    ; (or nil keyword)

(defmethod initialize-instance :after ((obj prohibition) &rest initargs)
  (declare (ignore initargs))
  (with-slots (bar-width width label fill stroke filter layer) obj
    (setf bar-width (or bar-width (/ width 8)))
    (setf label  (and label (make-label label)))
    (setf fill   (make-fill   (or fill   *default-prohibition-fill*   *default-fill*)))
    (setf stroke (make-stroke (or stroke *default-prohibition-stroke* *default-stroke* :none)))
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-prohibition-filter* *default-filter*)))
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-prohibition-layer* *default-layer*))))
  obj)

(defmethod check ((obj prohibition) canvas dict)
  (with-slots (bar-width label fill stroke filter clip-path) obj
    (check-member bar-width :nullable nil :types number)
    (check-object label     canvas dict :nullable t   :class  label-info)
    (check-object fill      canvas dict :nullable nil :class   fill-info)
    (check-object stroke    canvas dict :nullable nil :class stroke-info)
    (check-member clip-path :nullable   t :types symbol)
    (check-member filter    :nullable   t :types keyword))
  ;; this method must call super class' one.
  (call-next-method))


;; override of group::draw-group
(defmethod draw-group ((obj prohibition) writer)
  (let* ((canvas (group-get-canvas obj))
         (width  (canvas-width  canvas)))
    (macrolet ((register-entity (entity)
                 `(check-and-draw-local-entity ,entity canvas writer)))
      (with-slots (bar-width label fill stroke filter clip-path) obj
        (let ((*current-clip-path* clip-path))
          (multiple-value-bind (r1 r2 pts)
              (prohibition-get-points (canvas-center canvas) width bar-width)
            (path `((:move-to               ,(nth 0 pts))
                    (:arc-to  ,r1 ,r1 0 0 0 ,(nth 1 pts))
                    (:arc-to  ,r1 ,r1 0 0 0 ,(nth 2 pts))
                    (:move-to               ,(nth 3 pts))
                    (:arc-to  ,r2 ,r2 0 0 0 ,(nth 4 pts))
                    (:line-to               ,(nth 5 pts))
                    (:move-to               ,(nth 6 pts))
                    (:arc-to  ,r2 ,r2 0 0 1 ,(nth 7 pts))
                    (:line-to               ,(nth 8 pts)))
                  :stroke stroke :fill (make-fill :base fill :rule :evenodd) :filter filter))
          ;; draw label
          (when label
            (draw-label label obj clip-path writer))))))
  nil)


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro prohibition
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{prohibition}} position size ${KEY} width pivot fill stroke label link rotate layer filter id
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `size` ---- 禁止マークのサイズを数値で指定します。
;;* `bar-width` ----  禁止マークのバーの太さを数値で指定します。省略した場合は `size` の 8 分の 1 が使用されます。
;;* `pivot` ----  基準点が禁止マークのどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
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
;;　禁止マークを描画します。複数の禁止マークでスタイルを統一したい場合、with-prohibition-options マクロを
;;使うことができます。
;;
;;${SEE_ALSO}
;;
;;* 禁止マーク
;;* with-prohibition-options マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :prohibition
 |#
(defmacro prohibition (position size
                         &key bar-width pivot fill stroke label link rotate layer filter id)
  `(register-entity (make-instance 'kaavio:prohibition
                                   :bar-width ,bar-width
                                   :label (prohibition-fix-spec ,label)
                                   :fill ,fill :stroke ,stroke :filter ,filter
                                   :clip-path *current-clip-path*
                    #| group  |#   :position ,position :pivot ,pivot
                                   :width ,size :height ,size
                    #| shape  |#   :link ,link :rotate ,rotate
                    #| entity |#   :id ,id :layer ,layer)))



;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-prohibition-options
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-prohibition-options}} (${KEY} fill stroke filter layer) ${BODY} body
;;
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　prohibition マクロで描画される禁止マークのデフォルトオプションを変更します。キーワードパラメータ
;;群の説明は prohibition マクロを参照してください。
;;
;;${SEE_ALSO}
;;
;;* 禁止マーク
;;* prohibition マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-prohibition-options
 |#
(defmacro with-prohibition-options ((&key (fill   nil fill-p)
                                          (stroke nil stroke-p)
                                          (filter nil filter-p)
                                          (layer  nil layer-p)) &rest body)
  (let ((bindings nil))
    (labels ((impl (arg-p binding)
               (when arg-p (push binding bindings))))
      (impl fill-p   `(*default-prohibition-fill*   (make-fill2   *default-prohibition-fill*   ,fill)))
      (impl stroke-p `(*default-prohibition-stroke* (make-stroke2 *default-prohibition-stroke* ,stroke)))
      (impl filter-p `(*default-prohibition-filter* ,filter))
      (impl layer-p  `(*default-prohibition-layer*  ,layer)))
    `(let ,(nreverse bindings)
       ,@body)))
