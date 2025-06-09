#|
#|ASD|#                (:file "explosion"                 :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "polygon"
#|ASD|#                                                                "filter"
#|ASD|#                                                                "text-shape"))
#|EXPORT|#                ;explosion.lisp
 |#

(in-package :kaavio)

#|
#|EXPORT|#                :*default-explosion-font*
#|EXPORT|#                :*default-explosion-fill*
#|EXPORT|#                :*default-explosion-stroke*
#|EXPORT|#                :*default-explosion-filter*
#|EXPORT|#                :*default-explosion-layer*
 |#
(defparameter *default-explosion-font*         nil)
(defparameter *default-explosion-fill*         nil)
(defparameter *default-explosion-stroke*       nil)
(defparameter *default-explosion-filter*       nil)
(defparameter *default-explosion-layer*        nil)


(defun explosion-get-points (pattern w h)
  (cond
    ((= pattern 1) `((,(* w 0.02) ,(* h 0.10))
                     (,(* w 0.22) ,(* h 0.35))
                     (,(* w 0.01) ,(* h 0.39))
                     (,(* w 0.18) ,(* h 0.54))
                     (,(* w 0.01) ,(* h 0.67))
                     (,(* w 0.27) ,(* h 0.65))
                     (,(* w 0.23) ,(* h 0.80))
                     (,(* w 0.36) ,(* h 0.73))
                     (,(* w 0.39) ,(* h 0.99))
                     (,(* w 0.49) ,(* h 0.69))
                     (,(* w 0.62) ,(* h 0.91))
                     (,(* w 0.66) ,(* h 0.67))
                     (,(* w 0.84) ,(* h 0.83))
                     (,(* w 0.78) ,(* h 0.59))
                     (,(* w 0.99) ,(* h 0.61))
                     (,(* w 0.81) ,(* h 0.49))
                     (,(* w 0.98) ,(* h 0.38))
                     (,(* w 0.77) ,(* h 0.34))
                     (,(* w 0.85) ,(* h 0.21))
                     (,(* w 0.65) ,(* h 0.25))
                     (,(* w 0.67) ,(* h 0.00))
                     (,(* w 0.50) ,(* h 0.27))
                     (,(* w 0.39) ,(* h 0.12))
                     (,(* w 0.34) ,(* h 0.29))))
    ((= pattern 2) `((,(* w 0.21) ,(* h 0.17))
                     (,(* w 0.25) ,(* h 0.36))
                     (,(* w 0.05) ,(* h 0.38))
                     (,(* w 0.18) ,(* h 0.54))
                     (,(* w 0.00) ,(* h 0.59))
                     (,(* w 0.15) ,(* h 0.71))
                     (,(* w 0.06) ,(* h 0.82))
                     (,(* w 0.22) ,(* h 0.84))
                     (,(* w 0.23) ,(* h 1.00))
                     (,(* w 0.35) ,(* h 0.83))
                     (,(* w 0.40) ,(* h 0.91))
                     (,(* w 0.46) ,(* h 0.80))
                     (,(* w 0.54) ,(* h 0.87))
                     (,(* w 0.56) ,(* h 0.73))
                     (,(* w 0.69) ,(* h 0.80))
                     (,(* w 0.68) ,(* h 0.66))
                     (,(* w 0.87) ,(* h 0.72))
                     (,(* w 0.76) ,(* h 0.57))
                     (,(* w 0.85) ,(* h 0.52))
                     (,(* w 0.78) ,(* h 0.44))
                     (,(* w 1.00) ,(* h 0.31))
                     (,(* w 0.76) ,(* h 0.30))
                     (,(* w 0.83) ,(* h 0.14))
                     (,(* w 0.67) ,(* h 0.27))
                     (,(* w 0.68) ,(* h 0.00))
                     (,(* w 0.53) ,(* h 0.20))
                     (,(* w 0.45) ,(* h 0.09))
                     (,(* w 0.40) ,(* h 0.30))))
     (t nil)))

  

;;------------------------------------------------------------------------------
;;
;; class explosion
;;
;;------------------------------------------------------------------------------
(defclass explosion (text-shape)
  ((pattern :initform nil :initarg :pattern)  ; number
   (filter  :initform nil :initarg :filter))) ; (or nil keyword)
  
(defmethod initialize-instance :after ((exp explosion) &rest initargs)
  (declare (ignore initargs))
  (with-slots (layer filter) exp
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-explosion-layer* *default-layer*)))
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-explosion-filter* *default-filter*))))
  exp)
   
(defmethod check ((exp explosion) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (pattern filter) exp
    (check-member pattern :nullable nil :types number)
    (check-member filter  :nullable   t :types keyword))
  nil)

;; override of group::draw-group
(defmethod draw-group ((exp explosion) writer)
  (let* ((canvas (group-get-canvas exp))
         (width  (canvas-width  canvas))
         (height (canvas-height canvas)))
    (macrolet ((register-entity (entity)
                 `(check-and-draw-local-entity ,entity canvas writer)))
      (with-slots (pattern fill stroke filter clip-path) exp
        ;; draw 
        (let ((*current-clip-path* clip-path))
          (polygon (explosion-get-points pattern width height)
                                         :stroke stroke :fill fill :filter filter)))))
  ;; draw text
  (call-next-method))

;; no override.
;(defmethod text-shape-calc-size ((exp explosion))
;  (call-next-method))

;; no override.
;(defmethod text-shape-paragraph-area ((exp explosion))
;  (call-next-method))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro explosion1
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{explosion1}} position width height text ${KEY} pivot font fill stroke link rotate layer id filter contents
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `position` ----  描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `width` ----  幅を数値で指定します。
;;* `height` ----  高さを数値で指定します。
;;* `text` ----  内部に描画するテキストを文字列で指定します。改行は "~%" で表現します。
;;* `pivot` ----  基準点が爆発のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `font` ---- フォントを指定します。
;;* `fill` ---- 内部の塗り潰しを指定します。
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
;;　爆発を描画します。複数の爆発でスタイルを統一したい場合、with-explosion-options マクロを
;;使うことができます。
;;
;;${SEE_ALSO}
;;
;;* 爆発
;;* explosion2 マクロ
;;* with-explosion-options マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :explosion1
 |#
(defmacro explosion1 (position width height text
                         &key pivot font fill stroke link rotate layer id filter contents)
  (let ((code `(register-entity (make-instance 'explosion
                                               :pattern 1 :position ,position :pivot ,pivot
                                               :width ,width :height ,height
                                               :text ,text
                                               :align  :center :valign :center
                                               :font   (or ,font   *default-explosion-font*)
                                               :fill   (or ,fill   *default-explosion-fill*)
                                               :stroke (or ,stroke *default-explosion-stroke*)
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
;;#### macro explosion2
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{explosion2}} position width height text ${KEY} pivot font fill stroke link rotate layer id filter contents
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `position` ----  描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `width` ----  幅を数値で指定します。
;;* `height` ----  高さを数値で指定します。
;;* `text` ----  内部に描画するテキストを文字列で指定します。改行は "~%" で表現します。
;;* `pivot` ----  基準点が爆発のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `font` ---- フォントを指定します。
;;* `fill` ---- 内部の塗り潰しを指定します。
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
;;　爆発を描画します。複数の爆発でスタイルを統一したい場合、with-explosion-options マクロを
;;使うことができます。
;;
;;${SEE_ALSO}
;;
;;* 爆発
;;* explosion1 マクロ
;;* with-explosion-options マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :explosion2
 |#
(defmacro explosion2 (position width height text
                         &key pivot font fill stroke link rotate layer id filter contents)
  (let ((code `(register-entity (make-instance 'explosion
                                               :pattern 2 :position ,position :pivot ,pivot
                                               :width ,width :height ,height
                                               :text ,text
                                               :align  :center :valign :center
                                               :font   (or ,font   *default-explosion-font*)
                                               :fill   (or ,fill   *default-explosion-fill*)
                                               :stroke (or ,stroke *default-explosion-stroke*)
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
;;#### macro with-explosion-options
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-explosion-options}} (${KEY} font fill stroke filter layer) ${BODY} body
;;
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　explosion1 マクロおよび explosion2 マクロで描画される爆発のデフォルトオプションを変更
;;します。キーワードパラメータ群の説明は explosion1 マクロを参照してください。
;;
;;${SEE_ALSO}
;;
;;* 爆発
;;* explosion1 マクロ
;;* explosion2 マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-explosion-options
 |#
(defmacro with-explosion-options ((&key (font   nil font-p)
                                        (fill   nil fill-p)
                                        (stroke nil stroke-p)
                                        (filter nil filter-p)
                                        (layer  nil layer-p)) &rest body)
  (let ((bindings nil))
    (labels ((impl (arg-p binding)
               (when arg-p (push binding bindings))))
      (impl font-p   `(*default-explosion-font*   (make-font2   *default-explosion-font*   ,font)))
      (impl fill-p   `(*default-explosion-fill*   (make-fill2   *default-explosion-fill*   ,fill)))
      (impl stroke-p `(*default-explosion-stroke* (make-stroke2 *default-explosion-stroke* ,stroke)))
      (impl filter-p `(*default-explosion-filter* ,filter))
      (impl layer-p  `(*default-explosion-layer*  ,layer)))
    `(let ,(nreverse bindings)
       ,@body)))
